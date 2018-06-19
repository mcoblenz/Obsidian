#!/usr/bin/perl

use strict;

for my $filename (@ARGV) {
    print "$filename\n";
    eval {
        run_test($filename);
    }; if ($@) {
        print "Error testing $filename: $@";
    }
}

sub run_test {
    my $filename = shift;

    my %properties = parse_obstest($filename);

    # Validate obstest file.
    die "Must specify Obsidian source file for client.\n" if not exists $properties{client};
    die "Client source file $properties{client} doesn't exist.\n" if not -e $properties{client};
    die "Must specify Obsidian source file for server.\n" if not exists $properties{server};
    die "Server source file $properties{server} doesn't exist.\n" if not -e $properties{server};

    # Compile files
    compile($properties{server}, '');
    compile($properties{client}, '--build-client');

    # Use gradle to create a JAR.
    my $gradlecmd = "gradle -b buildscript/build.gradle -Pmain=$properties{mainclass}";
    print $gradlecmd, "\n";
    my $gradleresult = system $gradlecmd;

    die "gradle build failed and returned $gradleresult\n" unless $gradleresult == 0;
}

sub parse_obstest {
    my $filename = shift;

    open my $testfile, "<", $filename or die "Cannot open $filename: $!\n";

    # Parse obstest file.
    my %properties = (expected => '');
    my @propnames; # remember order properties were declared for when we print them out
    while (my $line = <$testfile>) {
        if ($line =~ /(.+): ?(.+)/) {
            # If the line has a colon in it, parse as a key/value pair.
            $properties{$1} = $2;
            push @propnames, $1;
        } else {
            # Otherwise, assume it's part of the expected output.
            $properties{expected} .= $line;
        }
    }
    push @propnames, 'mainclass';
    push @propnames, 'expected';

    if (not exists $properties{mainclass}) {
        # You can specify the main class name by adding 'mainclass: <whatever>' to
        # the obstest file; otherwise, it will use the server file name with
        # the extension stripped off.
        $properties{mainclass} = $properties{server};
        $properties{mainclass} =~ s{\.[^\.]+$}{};
    }

    # Get path to client and server files relative to testfile.
    my $dir = $filename;
    $dir =~ s{/[^/]*$}{/}; # strip off everything following last slash in filename
    $properties{client} = $dir . $properties{client}; # prepend dir name to source files
    $properties{server} = $dir . $properties{server};

    for my $k (@propnames) {
        printf "%10s => $properties{$k}\n", $k;
    }

    return %properties;
}

sub compile {
    # Run obsidian compiler on an .obs file.
    my ($file, $flags) = @_;
    my $compile = qq(sbt "run --verbose $flags --dump-debug obs_output)
                 .qq( --output-path obs_output $file");
    print "$compile\n";
    my $result = `$compile`;

    # I am unsure how to figure out if we succeeded or not -- sbt says 'success' a lot
    # when it doesn't succeed, and it doesn't seem to do anything w/ return codes.
    die "sbt did not say 'success':\n$result\n" unless $result =~ /success.+Total time/;
    die "sbt said 'error':\n$result\n" if $result =~ /error/i;
}

