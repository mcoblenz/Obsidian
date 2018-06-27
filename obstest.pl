#!/usr/bin/perl

use strict;
use warnings;

my $usage = qq{usage: $0 [-v] files
    -v: verbose -- show output from sbt and other commands\n};

if (!@ARGV) {
    die $usage;
}

# Check if the first argument was '-v'.
my $verbose = 0;
if ($ARGV[0] eq "-v") {
    $verbose = 1;
    shift @ARGV;
}

my @failed_tests;
my $passed_tests = 0;
my $total = $#ARGV+1;

# Test each file passed in on command line.
for my $filename (@ARGV) {
    print "$filename\n";
    # catch any errors thrown during the test.
    eval {
        do_test($filename);
    }; if ($@) {
        print "Test $filename failed: $@";
        push @failed_tests, $filename;
    } else {
        print "Test $filename passed!\n";
        $passed_tests ++;
    }
}

print "Passed $passed_tests out of $total tests.\n";

if (@failed_tests) {
    print "Failed: ", (join ", ", @failed_tests), "\n";
    exit 1;
} else {
    exit 0;
}

sub do_test {
    my $filename = shift;

    # Parse test file.
    my ($namesref, $serversref, $propsref) = parse_obstest($filename);
    # (unpack returned data structures into real variables. Perl)
    my @servernames = @$namesref;
    my %servers = %$serversref;
    my %props = %$propsref;

    die "Must specify a file to designate as the client.\n" unless exists $servers{client};

    # Since we process client differently, we take its filename out of the general list
    # of servers and put it into its own variable.
    my $client = $servers{client};
    delete $servers{client};

    # Compile files
    compile($client, '--build-client');
    for my $key (@servernames) {
        compile($servers{$key}, '');
    }

    # Run the client and servers and connect them up appropriately.
    my $output = run_test($client, \@servernames, \%servers);

    # Check if there was any difference.
    my $diff = output_diff($output, $props{expected});

    if (length $diff > 0) {
        die "Output wasn't what we expected:\n$diff\n";
    }
}

sub parse_obstest {
    my $filename = shift;

    open my $testfile, "<", $filename or die "Cannot open $filename: $!\n";

    # Parse list of files.
    my %files;
    # Perl hashes are unordered, but order is important here, so we need to
    # keep track of it ourselves.
    my @filenames;

    # Get path to obs files relative to testfile.
    my $dir = $filename;
    $dir =~ s{/[^/]*$}{/}; # strip off everything following last slash in filename
    while (my $line = <$testfile>) {
        # Strip trailing newline, and leading and trailing whitespace
        chomp $line;
        $line =~ s/^ +//;
        $line =~ s/ +$//;
        if ($line =~ /(.+): ?(.+)/) {
            # If the line has a colon in it, parse as a key/value pair.
            $files{$1} = $dir . $2;
            push @filenames, $1 unless $1 eq 'client';
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

    # Read the rest of the file as 'expected output.'
    while (my $line = <$testfile>) {
        $props{expected} .= $line;
    }

    for my $k (@filenames) {
        printf "%10s => %s\n", $k, $files{$k};
    }
    printf "    client => $files{client}\n";

    return \@filenames, \%files, \%props;
}

sub compile {
    # Run obsidian compiler on an .obs file.
    my ($file, $flags) = @_;
    my $compile = qq(sbt "run --verbose $flags --dump-debug obs_output)
                 .qq( --output-path obs_output $file");
    print "$compile\n";
    my $result = `$compile`;

    # Make sure the file actually compiled.
    die "compiler returned exit status $?:\n$result\n" if $? != 0;

    print $result if $verbose;
}

sub run_test {
    my $client = shift;
    my @servernames = @{shift @_};
    my %servers = %{shift @_};

    # Figure out names of JARs produced by compiler.
    my %jars;
    for my $key (@servernames) {
        $jars{$key} = jarname($servers{$key});
    }
    my $clientjar = jarname($client);

    # Fork a new process to run each non-client file.
    my @pids;
    my $port = 5566;
    for my $key (@servernames) {
        if (my $child = fork) {
            # Keep track of the PID of the child process.
            push @pids, $child;
            $port ++;
        } else {
            # Start the server in child process.
            print "java -jar $jars{$key} localhost $port\n";

            # Hide output from server process unless we're in verbose mode.
            open STDOUT, ">", "/dev/null" or die "$!" unless $verbose;

            exec 'java', '-jar', $jars{$key}, 'localhost', $port;
        }
    }

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
    my $output = `$cmd`;

    print "Client output:\n", $output if $verbose;

    kill 'HUP', @pids;

    print "Waiting for child processes to shut down.\n" if $verbose;

    # Wait for all child processes to finish.
    1 while wait() != -1;

    return $output;
}

sub jarname {
    # Figure out what the names of the JAR files will probably be.
    # Replace file extensions with '.jar', capitalize the filename,
    # and remove preceding directories.
    my $name = shift;
    $name =~ s{/?([^/]+/)+}{}; # strip leading directories
    $name =~ s{\.[^\.]+$}{.jar}; # replace file extension with .jar
    $name = ucfirst $name; # capitalize first letter
    return $name;
}

sub output_diff {
    my ($output, $expected) = @_;

    # Compare expected output to test output by writing them out to files
    # and running 'diff'.
    open my $file1, ">", "test-output";
    print {$file1} $output;

    open my $file2, ">", "expected-output";
    print {$file2} $expected;

    my $diff = `diff test-output expected-output`;

    # Delete the temporary files.
    close $file1;
    close $file2;

    unlink 'test-output', 'expected-output';

    return $diff;
}
