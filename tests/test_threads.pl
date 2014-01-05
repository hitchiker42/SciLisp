#!/usr/bin/perl
use v5.14.0;
#fork a child process to loop SciLisp running basic tests
#to test that threads work consistantly
my $test_time = @ARGV[0];
my $pid = fork;
if($pid >0){  
  #parent process
  eval{
    local $SIG{ALRM} = sub {die "TIMEOUT"};
    alarm $test_time;
    waitpid($pid,0);
    exit($?);
  };
}
elsif ($pid == 0){
  #child process
  exec '/bin/bash','thread_test_loop.sh';
  exit(99);
}
