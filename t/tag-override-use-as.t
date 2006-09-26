#!perl

BEGIN { chdir 't' if -d 't' }

use strict;
use warnings;

use Test::More tests => 2;
use Text::MediawikiFormat as => 'wikiformat';

my $wikitext =<<WIKI;

* This should be a list.

# This should be an ordered list.

** This is like the default unordered list
** But not indented

! This is like the default unordered list
! But marked differently

WIKI

my %format_tags = (blocks => {unordered => qr/^!\s*/});
 
my $htmltext = wikiformat ($wikitext, \%format_tags, {});
like ($htmltext, qr!<li>But marked differently</li>!m,
      'redefining a list type works with use as');

%format_tags = (
	indent => qr//,
	blocks => { 
		ordered         => qr/^#\s*/, 
		unordered       => qr/^\*\s*/
	},
	indented => {unordered => 0},
); 

$htmltext = wikiformat ($wikitext, \%format_tags, {});
like ($htmltext, qr!<li>\* But not indented!m,
      'redefining a list type to require no indent works with use as');
