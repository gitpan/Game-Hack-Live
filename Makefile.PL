use 5.008008;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'Game::Hack::Live',
    VERSION_FROM      => 'lib/Game/Hack/Live.pm', # finds $VERSION
    PREREQ_PM         => { Expect => 1.20 },
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/Game/Hack/Live.pm', # retrieve abstract from module
       AUTHOR         => 'Ph. Marek <pmarek@cpan.org>') : ()),
);
