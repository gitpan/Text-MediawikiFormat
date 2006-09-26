#!perl

BEGIN { chdir 't' if -d 't' }

use strict;
use warnings;

use Test::More tests => 3;
use Text::MediawikiFormat;

my $wikitext =<<WIKI;
StudlyCaps

WIKI

my %opts = ( 
	prefix => 'rootdir/wiki.pl?page=',
);

my $htmltext = Text::MediawikiFormat::format($wikitext, {}, \%opts );
unlike $htmltext, qr!<a href='rootdir/wiki\.pl\?page=StudlyCaps'>!m,
       'should create links from StudlyCaps if implicit_links is left alone';

$opts{implicit_links} = 0;
$htmltext = Text::MediawikiFormat::format($wikitext, {}, \%opts );
unlike ($htmltext, qr!<a href='rootdir/wiki\.pl\?page=StudlyCaps'>!m,
	'...and if implicit_links set to 0');

$opts{implicit_links} = 1;
$htmltext = Text::MediawikiFormat::format($wikitext, {}, \%opts );
like ($htmltext, qr!<a href='rootdir/wiki\.pl\?page=StudlyCaps'>!m,
      '...and if implicit_links set to 0');
