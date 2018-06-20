#!/usr/bin/perl

use strict;

for my $filename (@ARGV) {
    print "$filename\n";
    eval {
        do_test($filename);
    }; if ($@) {
        print "Error testing $filename: $@";
    }
}

sub do_test {
    my $filename = shift;

    # Parse test file.
    my ($namesref, $serversref, $propsref) = parse_obstest($filename);
    my @servernames = @$namesref;
    my %servers = %$serversref;
    my %props = %$propsref;

    die "Must specify a file to designate as the client.\n" unless exists $servers{client};

    my $client = $servers{client};
    delete $servers{client};

    # Compile files
    compile($client, '--build-client');
    for my $key (@servernames) {
        compile($servers{$key}, '');
    }

    # Run the client and servers and connect them up appropriately.
    my $output = run_test($client, \@servernames, \%servers);

    # Compare expected output to test output.
    open my $file1, ">", "test-output";
    print {$file1} $output;

    open my $file2, ">", "expected-output";
    print {$file2} $props{expected};

    my $diff = `diff test-output expected-output`;

    close $file1;
    close $file2;

    unlink 'test-output', 'expected-output';

    if (length $diff > 0) {
        print $diff;
    } else {
        print "No difference from expected output!\n";
    }
}

sub parse_obstest {
    my $filename = shift;

    open my $testfile, "<", $filename or die "Cannot open $filename: $!\n";

    # Parse list of files.
    my %servers;
    # Perl hashes are unordered, but order is important here, so we need to
    # keep track of it ourselves.
    my @servernames;

    # Get path to obs files relative to testfile.
    my $dir = $filename;
    $dir =~ s{/[^/]*$}{/}; # strip off everything following last slash in filename
    while (my $line = <$testfile>) {
        chomp $line;
        $line =~ s/^ +//;
        $line =~ s/ +$//;
        if ($line =~ /(.+): ?(.+)/) {
            # If the line has a colon in it, parse as a key/value pair.
            $servers{$1} = $dir . $2;
            push @servernames, $1 unless $1 eq 'client';
        } elsif ($line eq '***') {
            # When we reach a line that's just '***', we break out of this loop
            # and put everything else as expected output.
            last;
        } elsif (length $line > 0) {
            print "Unable to parse line: $line\n";
        }
    }

    # Special properties we're storing
    my %props = (expected => '');

    while (my $line = <$testfile>) {
        # Read the rest of the file.
        $props{expected} .= $line;
    }

    for my $k (@servernames) {
        printf "%10s => %s\n", $k, $servers{$k};
    }
    printf "    client => $servers{client}\n";

    return \@servernames, \%servers, \%props;
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

sub run_test {
    my ($client, $namesref, $serversref) = @_;
    my @servernames = @$namesref;
    my %servers = %$serversref;
    # Replace file extensions with '.jar',
    # and remove preceding directories.
    my %jars;
    for my $key (@servernames) {
        $jars{$key} = $servers{$key};
        $jars{$key} =~ s{/?([^/]+/)+}{}; # strip leading directories
        $jars{$key} =~ s{\.[^\.]+$}{.jar}; # change file extension
    }
    my $clientjar = $client;
    $clientjar =~ s{/?([^/]+/)+}{};
    $clientjar =~ s{\.[^\.]+$}{.jar};

    # Fork a new process to run each non-client file.
    my @pids;
    my $port = 5566;
    for my $key (@servernames) {
        if (my $child = fork) {
            push @pids, $child;
            $port ++;
        } else {
            # Start the server
            print("java -jar $jars{$key} localhost $port\n");
            exec('java', '-jar', $jars{$key}, 'localhost', $port);
        }
    }

    my $output;
    # Wait for the server to start...
    sleep 1;
    # Connect to the server(s)
    $port = 5566;
    my $cmd = "java -jar $clientjar ";
    for my $key (@servernames) {
        # Pass the port of each server file to the client.
        $cmd .= "localhost $port ";
        $port ++;
    }
    print $cmd, "\n";
    $output = `$cmd`;

    kill 'HUP', @pids;
    sleep 1; # wait for processes to shut down

    return $output;
}
