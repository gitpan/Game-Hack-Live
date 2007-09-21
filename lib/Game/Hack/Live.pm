#!/usr/bin/perl
# vim: set sw=2 expandtab : #

use Expect;

$VERSION=0.3;


# Client program name
our($prg,
# Client PID
    $prg_pid,
# Timeout for GDB's answers
    $TIMEOUT,
# The Expect object connected to GDB
    $gdb,
# The STDIN Expect object
    $input,
# Whether the program should currently be running
    $should_be_running,
# Which extra strings we look for
    $callbacks,
# Patch commands
    @summary,
# Base path for file dumps
    $dumppath,
# Dump start number
    $dump,
# GDB Prompt variables
    $gdb_prompt_base,
    $gdb_prompt_count);


Inits(@ARGV);
StartDebuggee();
GDBStart();


#	For debugging ...
# $gdb->log_file("/tmp/expect-log", "w");
# $gdb->debug(2);


$quit=0;
while (!$quit)
{
  print "---> ";
  (undef, $error) = expect( undef, 
      '-i', [ $input ], 
      [ qr/^\s*dumpall(?:\s+(.+))?/i, \&DumpAll, ],
      [ qr/^\s*#/, sub { return 0; print "comment ignored\n"; 0; },  ],
      [ qr/^\s*cleanup/i, \&CleanUp, ],
      [ qr/^\s*find\s+(\S+)/i, \&Find, ],
      [ qr/^\s*keepvalueat\s+(\S+)(?:\s+|\s*=\s*)(\S+)(?:\s+["'](.+?)["'])?/i, \&KeepValueAt, ],
      [ qr/^\s*killwrites\s+(\S+)(\s+(["'])(.+)\3)?/i, \&KillWrites, ],
      [ qr/^\s*(.+?)\s*$/, \&PassThru, ],
      [ 'eof', sub { $quit=1; 0; },  ],
      '-i', [ $gdb ],
      GDBmatches(1),
      );
}

print "\n\nQuitting ...\n";
$gdb->send("kill\n");
$gdb->send("q\n");
$gdb->hard_close();

# Print results.
print @summary;

exit;

###################################   The end. 
###################################****************************


###################################****************************
### ### ### ### ### ### ### ### ###   Initialization functions

# Set initial variables.
sub Inits
{
  my(@args)=@_;

  $input = Expect->init(\*STDIN);

#  $SIG{"INT"} = sub { 
#    $should_be_running=0; 
#    DebuggeeBreak(); 
#    print "\nDebuggee stopped.\n"; 
#  };

# Could we do the conversion using gdb?
# We can find out how many bytes that type has (print sizeof(...)),
# but how do we get the representation? (for searching)
# not used yet
  %type_convs = (
      "long" => "l",
      "unsigned long" => "L",
      "int" => "i",
      "unsigned int" => "I",
      "unsigned" => "I",
      "signed char" => "c",
      "char" => "C",
      "unsigned char" => "C",
      "long long" => "q",
      "unsigned long long" => "Q",
      "float" => "f",
      "double" => "d",
      "long double" => "D",
      );

  $|=1;

  $prg=shift(@args); 
  $TIMEOUT=5; 
  $should_be_running=0;
  $callbacks=();
  $dumppath="/tmp";
  $dump=0;
  $gdb_prompt_base="GDB-delim-$$-" . time;
  $gdb_prompt_count="a000000";
}


# Start the program to-be-debugged
sub StartDebuggee
{
  $prg_pid=fork();
  die if (!defined($prg_pid));
  exec($prg) || die if (!$prg_pid);

  print "Started $prg with pid $prg_pid\n";
}


# Start gdb, and attach to debuggee
sub GDBStart
{
  $gdb = new Expect;
  $gdb->raw_pty(1);
  $gdb->log_stdout(0);
  $gdb->spawn("gdb 2>&1", ()) or die "Cannot spawn gdb: $!\n";


  GDBSync($gdb);
  GDBSend("attach $prg_pid");
  GDBSend("set pagination off");
  CleanUp();
  $should_be_running=1;
  DebuggeeCont();
}


###################################****************************
### ### ### ### ### ### ### ### ###   GDB I/O

# Returns an array with currently allowed matches
# $cbs: include callbacks?
sub GDBmatches
{
  my($cbs)=@_;
  my(@a);

  @a=();
  map { push @a, [ $_, @{$callbacks{$_}} ]; } keys %callbacks 
    if $cbs;

  push @a,
       [ $gdb_prompt_delimiter, sub { $is_running=0; }, ],
       [ "Continuing", sub { $is_running=1; }, ];
  return @a;
}


# Change the GDB Prompt, and re-sync
# The debuggee must be stopped when calling that.
sub GDBSync
{
  my($obj)=@_;

  $gdb_prompt_count++;
  $gdb_prompt_delimiter = $gdb_prompt_base . "-" . $gdb_prompt_count . "-ZZ";
  $obj->print_log_file("new prompt set to $gdb_prompt_delimiter\n");
  $obj->send("set prompt $gdb_prompt_delimiter\\n\n");
  $obj->expect($TIMEOUT, $gdb_prompt_delimiter);
}


# Look for the expected prompt, or other specified strings.
sub GDBPrompt
{
# $no_cbs??? TODO
  my(@others)=@_;
  my($obj);

  $obj=ref($others[0]) eq "Expect" ? shift(@others) : $gdb;
  @others=GDBmatches(1) if !@others;
  if (!$obj->expect($TIMEOUT, @others))

  {
    $obj->print_log_file(">>> NO MATCH ... expected $gdb_prompt_delimiter, Continuing, or @others\n");
    $obj->print_log_file("got ", $obj->clear_accum());
    die "No match\n";
  }
}


# Send a command to GDB, and wait for answer.
sub GDBSend
{
  my(@parm)=@_;
  my($stg, @mat);
  my($obj);

  $obj=ref($parm[0]) eq "Expect" ? shift(@parm) : $gdb;
  ($stg, @mat)=@parm;
  if ($stg)
  {
    $obj->print_log_file(">>> SENDING: $stg\n");
    $obj->send($stg . "\n");
    $obj->clear_accum();
  }
  GDBPrompt($obj, @mat);
  $obj->print_log_file(">>> after sending: run=$is_running ",
      "should=$should_be_running\n");
}


###################################****************************
### ### ### ### ### ### ### ### ###   User commands

# Unknown string - just pass to GDB
sub PassThru 
{ 
  my($self)=@_;
  my($cmd);


  $cmd=($self->matchlist())[0];
  $should_be_running=1 if $cmd =~ m#^\s*c(o(nt?)?)?#i;

  $gdb->print_log_file(">>> passing '$cmd' $is_running $should_be_running");
  return if ($should_be_running && $is_running);

  DebuggeeBreak($gdb);
  GDBSync($gdb);
  GDBSend($gdb, $cmd, GDBmatches(0)); 
  print $gdb->before,"\n";
  DebuggeeCont() if ($should_be_running);
  0; 
}


# Clean up search history
sub CleanUp
{
  @findlist=();
  %find_adr=();

  %callbacks=();

  DebuggeeBreak($gdb);
  GDBSend($gdb, "delete", GDBmatches(0)); 
  DebuggeeCont() if ($should_be_running);
}


# Find some value in the program
sub Find
{
  my($self)=@_;
  my($parm);
  my($ref, $bin, @most);

  $parm=($self->matchlist())[0]; 
# TODO: better conversion to a string/regex
  $bin=pack("l", $parm);

  DebuggeeBreak($gdb);

# use a reference, so that the callback can give us data.
  $ref=[];
  push @findlist,  [$parm, $bin, $ref];

  print "Searching for ", unpack("H*", $bin),"\n";
  $gdb->print_log_file("Searching for ". unpack("H*", $bin)."\n");

  forEachRWMem(\&FindCallBack, $bin, $ref);

# Count matches
  map { $find_adr{$_}++; } @$ref;
# Find best matches
  @most=sort { $find_adr{$b} <=> $find_adr{$a}; } keys %find_adr;

  print "\nMost wanted:\n  ";
  map { printf "0x%08X(%d)  ", $_,$find_adr{$_}; } @most[0 .. 5];
  print "\n";

  DebuggeeCont() if ($should_be_running);
}


# Register a watchpoint for the memory location, and when it's triggered
# kill the write command.
sub KillWrites
{
  my($self)=@_;
  my($adr, $name);

  $adr=oct(($self->matchlist())[0]);
  $name=($self->matchlist())[4];

  DebuggeeBreak();
  GDBSend("watch *(int*)$adr", GDBmatches(0));
  $gdb->before =~ m#(watchpoint \d+:)#i || die;
  if (!$1)
  {
    print "Watchpoint could not be set:\n",$gdb->before,"\n";
    return;
  }

  print "Using ", $1," killing writes to $adr.\n";

  $gdb->print_log_file("Registering callback for '$1':");
  $callbacks{$1}= [ \&KillWriteCallBack, $adr, $name ];
  DebuggeeCont() if ($should_be_running);
}


# Register a watchpoint for the memory location, and tell GDB to change the 
# value back.
sub KeepValueAt
{
  my($self)=@_;
  my($adr,$val,$name);
  my($l1, $l2, $l3);

  ($adr, $val, $name)=$self->matchlist();

  DebuggeeBreak();
  $l1="set *(int*)$adr=$val";
  $l2="watch *(int*)$adr";
  $l3=["commands",
    "silent",
    $l1,
    "c",
    "end"];
  GDBSend($l1, GDBmatches(0));
  GDBSend($l2, GDBmatches(0));
  $gdb->before =~ m#(watchpoint \d+:)#i;
  if (!$1)
  {
    print "Watchpoint could not be set:\n",$gdb->before,"\n";
    return;
  }

  print "Using ", $1," keeping value at $adr at $val.\n";

  $gdb->print_log_file("Registering actions for $name '$1':");
  GDBSend(join("\n",@$l3), GDBmatches(0));
  $gdb->print_log_file("Keeping $adr at $val -- $name\n");

  push @summary,
       sprintf("# keeping \"%s\" (0x%x) at 0x%x (%d):\n\t%s\n\n",
           $name, $adr, $val, $val, join("\n\t", $l1, $l2, @$l3));

  DebuggeeCont() if ($should_be_running);
}


# Dump each writeable memory area to a distinct file.
sub DumpAll
{
  my($self)=@_;
  my($dir, $desc, $c);

  $desc=($self->matchlist())[0];

  $dir=sprintf("%s/%d-%04d-%s", $dumppath, $prg_pid, $dump, $desc);
  mkdir($dir) || die $!;
  $dump++;
  print "dumping into $dir...\n";

  $c=forEachRWMem(\&SaveMem, $dir);
  print "Dumped $c mappings.\n";
  0;
}


###################################****************************
### ### ### ### ### ### ### ### ###   Break/Continue Debuggee

sub DebuggeeCont
{
  my($obj)=@_;
  return if ($is_running);

  $obj=$gdb unless $obj;

  $obj->print_log_file("continuing: is_running=$is_running\n");
  $obj->print_log_file(">>> continuing\n");
  GDBSend($obj, "c", GDBmatches(0));
}


sub DebuggeeBreak
{
  my($obj)=@_;
  my(@a, $i);

  $obj=$gdb unless $obj;

  $obj->print_log_file("Try to stop: is_running=$is_running\n");
  return if (!$is_running);

  $i=1;
  $obj->print_log_file(">>> callstack($i): ", join("; ", @a), "\n"), $i++
    while (@a = caller($i));

  $obj->print_log_file(">>> Stopping program via signal\n");
  kill "INT", $prg_pid;
  $obj->clear_accum();
  $is_running=0;
  select(undef, undef, undef, 0.01);
#	GDBSend(undef, "Program received signal");
  GDBSync($obj);
  print $obj->clear_accum();
  $obj->print_log_file("Should be stopped: is_running=$is_running\n");
}


###################################****************************
### ### ### ### ### ### ### ### ###   Utility Functions

# Write some memory block to a file.
sub SaveMemtoFile
{
  my($start, $end, $name)=@_;

  $gdb->print_log_file(
      sprintf(">>> dumping 0x%08x to 0x%08x into %s\n",
        $start, $end, $name));
  GDBSend("dump binary memory $name $start $end", GDBmatches(0));
}


# Wrapper function.
sub SaveMem
{
  my($start, $end, $dir)=@_;

  SaveMemtoFile($start, $end, "$dir/$start-$end");
}


# Worker function for KillWrites
sub KillWriteCallBack
{
  my($exp, $data_adr, $name)=@_;
  my($adr, $p1, $p2, $l, $cmd);
  my($killer);

# When we get here, the program has already stopped.
  $is_running=0;
  $exp->clear_accum();
  $exp->print_log_file("### Got callback!\n");
  DebuggeeBreak($exp);
# resync
  GDBSync($exp);

  GDBSend($exp, "info program", GDBmatches(0)); 
  $exp->before =~ m#Program stopped at ((0x)?[0-9a-f]+)#i;
  $adr=oct($1);
  $exp->print_log_file("### Program at $adr!\n");
  GDBSync($exp);

# We need the instruction immediately before, as on Intel EIP is already 
# changed.
# Is there some more reliable way to get it? There may be aliasing issues ..
# Are there longer sequences than 16 Bytes?
# As a watchpoint works with memory, there must be a memory operand to this 
# instruction, so there should be *at least* 2 bytes.
# Less wouldn't work with the relative jump anyway - we'd need to put NOPs.
  $p1=0;
  for $aliasing (2 .. 16)
  {
# We start with some offset before, to let the disassembler synchronize.
    $startadr=$adr-$aliasing-32;
    GDBSend($exp, "disas $startadr " . ($adr+1), GDBmatches(0)); 

# GDB prints the EIP without leading zeroes (info program), but the 
# diassembly has it. Don't know now how it's printed on 64bit. 
# We fetch an array, and the interesting parts should be the last two ...
    @found = ($exp->before =~ m/\n(0x\w+)\s+\<\S+\>:\s+(.*)/mg);

# Look whether we had the correct address at the end (instruction aliasing 
# might prevent this)
    $p1=oct($found[-4]), $cmd = $found[-3] if (oct($found[-2]) == $adr);

# Length of instruction
    $l=$adr - $p1;
    $exp->print_log_file("### (aliasing $aliasing) $cmd: $p1-$adr=$l\n");
    last if ($p1);
  }

  die unless $p1;

# TODO: currently x86 only
# The JMP itself is 2 bytes - they are already counted by EIP.
  $killer=sprintf("set *(short*)(0x%X)=0x%02Xeb", $p1, $l-2); 
  GDBSend($exp, $killer, GDBmatches(0));

# Remove tabs and similar
  $cmd =~ s/\s+/ /g;

  printf "\nKilled a write to 0x%X (%s): %s\n",
  $adr, $name, $cmd;

  push @summary,
       sprintf('# stopped at 0x%x for "%s" (at 0x%x); killing command "%s"' .
           "via\n\t%s\n\n",
           $adr, $name, $data_adr, $cmd, $killer);

  DebuggeeCont() if ($should_be_running);
  exp_continue; 
}


# Main worker function for Find
sub FindCallBack
{
  my($start, $end, $bin, $foundref)=@_;
  my($size, $buff, $offset, $l, $pos, $np, $adr, $blen);
  my($tmp)="/tmp/xxx.$$." . rand();
  my($count);

  $blen=length($bin);
  SaveMemtoFile($start, $end, $tmp);

# We read the (arbitrarily large) memory blobs in pieces, and keep
# the last few bytes from the previous round - in case the match would span 
# two such blocks.

# Size of each block
  $size=128 * 1024;
  $buff="";
  $offset=0;
  open(TMP, "< $tmp") || die "read $tmp: $!";
  while (1)
  {
    $l=sysread(TMP, $buff, $size, length($buff));
    $gdb->print_log_file(
        sprintf("searching for %s at 0x%x (%d) + 0x%x (%d)\n", 
          unpack("H*", $bin), 
          $start, $start, $offset, $offset));
    die $! unless defined($l);
    last if !$l;

    $pos=0;
# TODO: use regex
    while ( ($np=index($buff, $bin, $pos)) >=0)
    {
      $adr=$start+$offset+$np;

# Should we still print addresses?
      $tmp=($count++) -5;
      printf "  found at 0x%08x (0x%08x + 0x%x): %s\n", 
      $adr, $start, $np+$offset,
      unpack("H*", substr($buff, $np, 8)), 
      if ($tmp <0);

# Should we print "more here..."?
      printf "  ...\n", $adr 
        if ($tmp ==0);

      push @$foundref, $adr;
      $pos=$np+1;
    }

# We advance the offset ...
    $offset+=length($buff);
# and take only as much from the end as necessary for further matches
    $buff=substr($buff, -$blen);
# but that many characters were kept.
    $offset-=length($buff);
  }
  close(TMP);
  unlink $tmp;
}


# For each writeable memory, do ...
sub forEachRWMem
{
  my($func, @parm)=@_;
  local(*MAP);
  my($start, $end, $count);

  DebuggeeBreak();
# read writeable mappings
  open(MAP, "/proc/$prg_pid/maps") || die $!;
  $count=0;
  while (<MAP>)
  {
    ($start,$end) = m#^(\w+)-(\w+) rw.. #;
# virtual address 0 would be invalid anyway
    next unless $start; 

    &$func(oct("0x$start"), oct("0x$end"), @parm);
    $count++;
  }

  DebuggeeCont() if ($should_be_running);

  close MAP;
  return $count;
}


###################################****************************
### ### ### ### ### ### ### ### ###   Conversion Functions

# Should return a binary blob (for index()) or a regex describing the data 
# the user wants to find
# Not used currently
sub String2Bin
{
  my($stg)=@_;
  my($type, $val) = ($stg =~ m#^(?:\(?([a-z ]+)\)?)?\s*((?:0x?)?[0-9a-f\.])#);
# We do a fuzzy match, eg. for floating point values
# Eg. if only a few digits are shown.

# string?
  return $2 if (!$type) && $stg =~ m/^\s*(["'])(.*?)\1?$/;

  $type = "int" if !$type_convs{$type};

  return pack($type_convs{$type},$val);
}


###################################****************************
### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ###   Really the end.
### ### ### ### ### ### ### ### ###   Only documentation left.
### ### ### ### ### ### ### ### ###
###################################****************************

__END__

=head1 NAME

Game::Hack::Live - Perl script to ease playing games

=head1 SYNOPSIS

To start the script:

perl -MGame::Hack::Live -e0 <name of executable>

Commands for the script:

dumpall [name]
find <value>
cleanup
keepvalueat <address> <value> ["textual name"]
killwrites <address> ["textual name"]

All other strings are passed through to GDB.


=head1 DESCRIPTION

This script helps you patch your favourite programs, to keep them from
incrementing error counters or decrementing life values.

It does this by starting your program, and attaching C<gdb> (the GNU 
    debugger) to it; with its help it can examine memory, change it, and find 
locations in the program that try to modify it.

=head1 SOME DETAILS

=head2 Controlling the run-state - C<CTRL-C> and C<SIGINT>, "C<cont>"

To control whether the debuggee should run or not you can simply press 
CTRL-C; the resulting signal gets caught by the script, and it will try to 
stop the debuggee, so that its state can be examined.

Use any abbreviation of C<cont> (like eg. C<c>) to continue its execution.

=head2 C<dumpall>

dumpall [name]

This command writes all writeable mappings of the program into files like 
F</tmp/$PID-$DUMP_NUMBER-$NAME/$start-$end>.

These could be used to compare the data at different times.

=head2 C<find>

  find <value>

The most important step is to find the memory location where the debuggee 
stores the precious values. If you know that you have 982 money points 
left, you can simply say

  find 982

and you'll see a list of some memory locations where this value was found. 
If you buy something and see the number 922, use

  find 922

to see an updated list; especially the I<most wanted> list, where the number of 
matches is counted. If you typed C<find> 7 times, and one memory location 
was found every time, it's very likely that this is the address you want.

Normally 2 or 3 searches suffice to determine a location.

=head2 C<cleanup>

  cleanup

If you found an interesting memory location (and used it with the commands 
L</keepvalueat> or L</killwrites>, or wrote it down), you might want to 
start a new search.

Use the C<cleanup> command for that; it cleans the search history.

=head2 C<keepvalueat>

  keepvalueat <address> <value> ["textual name"]

If you found out where your money, life or wizard points are stored, you 
might want to keep that at a value that eases the game a bit. Simply tell 
which memory location should be kept at which value, and an optional name 
(which is used for L</Final output>), and you get a watchpoint registered 
that resets the value after it has been changed.

  keepvalueat 0xafad1208 20000 "Money"

Please note that this might cause a (very slight) runtime overhead, in that 
B<every write> to this location causes a break into C<gdb>, which 
overwrites the value again, and has to return to the debuggee.

Depending on the frequency of changes you might be able to notice that.

=head2 C<killwrites>

  killwrites <address> "<textual name>"

This command has a purpose similar to L</keepvalueat>, but achieves that by 
patching the program.

It registers a watchpoint, too; but on a write to the given address the 
script takes momentarily control, deassembles a bit of the program, and 
patches the write command so that the modified value doesn't reach its 
memory location.

  keepvalueat 0xafad1208 "Money"

=head2 Discussion about C<keepvalueat> and C<killwrites>

=over

=item *

L</killwrites> has to be done only for a single run; the patch commands 
might then simply be loaded without runtime-overhead. Even a modified 
binary might be written.

=item *

L</keepvalueat> gives a better starting point - instead of having to do some 
steps to get enough money you simply B<have> the money needed.

=back

Possibly both could be done - patching writes out of the binary, and change 
the initial value that gets loaded. Volunteers?

=head2 Final output

Currently after the script was ended with C<EOF> (C<CTRL-D> on the command 
line) it outputs the patching commands used.


=head1 SEE ALSO

The C<gdb> documentation for other useful commands, and I<Star Trek - 
TODO> about ethical considerations (Kirk patches a simulation, and so is 
the only one that ever made it).


=head1 BUGS/CAVEATS/TODO/IDEAS/WISHLIST

=over

=item Operating system - Linux only

I found no way to determine in C<gdb> which memory regions are mapped 
read-write (C<info proc mappings> doesn't show the mode), so I had to 
read F</proc/*/maps> directly - which limits this script to B<Linux only>
currently.

=item Stability

This is my first project using L<Expect>, which was recommended to me by 
Gabor Szabo (CPAN id SZABGAB) during the YAPC::Vienna 2007 -- instead of 
writing my own loop.

So there might be bugs; the script might break the connection, but the 
debuggee will run along.

You're welcome to help.

=item More types for searching

The B<searching> is currently done B<only for integer values>, although I'm 
working on supporting floating point values, too.

The problem is that a floating point value can't be matched exactly - if 
you're shown C<200> on the display, it could be anything from C<200.0> to 
C<200.999> -- and they have different binary representations.
So for floating point values a range must be given (or deduced, or 
assumed).

Probably it would be best to take a numeric value as any of C<double>, 
C<float> and C<int> (or C<long>) and use a regex for searching, instead of 
C<index()> as now.

=item Search intelligence

For some things it might be good (or even necessary) to avoid giving
distinct values to look for - eg. because they're simply not known.
If you have just some kind of barchart showing energy left, you might 
know when it changes, but no value. 
(Similar if the display differs from the internal representation).

So storing/comparing memory dumps might prove helpful for such cases.
First attempts done in L</dumpall> - we'd have to ask for two (or more) 
dumps with the interesting value unchanged, and a few with it changed - 
to compare the dumps and find the location.
(Which is the fastest way - simple use the dumps as bitvectors, XOR them, 
and look for 0/!0 values?)

=item Hardware (in)dependence

B<Patching> the write statements out of the code is currently valid for 
B<32bit x86 only>; via a C<jmp short> (C<0xeb>).

Hardware breakpoints (for the L</keepvalueat> and L</killwrites> 
commands) are available on the higher x86 (Pentium and above, I believe).

The number of available hardware breakpoints is not checked.

=item Binary matching

The commands given by L</killwrites> are meaningful only for a single 
executable; if it gets only recompiled, they might be off.

So this should maybe get bound to a MD5 of the binary or some such.

=item Binary patching, program start

Instead of simply outputting commands for C<gdb> to patch the program, the 
binary itself could be patched (to a new name); then this binary could 
simply be started instead of the other one. (Would avoid needing to check 
the MD5 of the binary.)

Or a shell script should be printed, that would take care of patching the 
binary (via C<gdb>) itself - so only the shell script would have to be 
started. (Could check for the MD5 of the executable, too!)

=item Updates

The region around the patched location could be stored as a disassembly 
dump, to possibly find the same program code again after an update.

=item More difficult - finding locations by output

As in the good old times (C64 and similar) sometimes the easiest way is to 
look for the output code - eg. search for C<Lifes: %4d  Energy: %3d> in the 
program data, do a cross-reference where it's used, and resolve back to 
the memory locations used for the output (eg. via C<printf>).

Would need some kind of intelligent disassembler - to (reversely) follow the 
data-stream; but should be doable, at least for easier things (like 
C<printf> output - simply look for argument I<N> on the stack, where it 
comes from).

Should be C<Game::Hack::Offline>, or some such.

=item Interface

Some kind of graphical interface would be nice (eg. Tk) - on another screen, X 
server, or some serial console?

=item Other points

As linux is getting address space randomizations, the data addresses reported 
might not be worth anything in the long run; if the executable sections get 
moved, too, not even the patch commands given by L</killwrites> will help.

There should be some way to describe the relative positioning of the memory 
segments - easy for C<heap>, C<stack> or executable segments, but other 
anonymous ranges?

=back

Patches are welcome.


=head1 AUTHOR

Ph. Marek <pmarek@cpan.org>


=head1 COPYRIGHT AND LICENSE

Copyright (C) 2007 by Ph. Marek;
licensed under the GPLv3.

=cut

