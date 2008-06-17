#!perl

BEGIN { chdir 't' if -d 't' }

use strict;
use warnings;

use Test::More tests => 9;
use Text::MediawikiFormat as => 'wf', process_html => 0;

my $wikitext =<<WIKI;

[Ordinary extended link]

[http://nowhere.com explicit URI]

[[Usemod extended link]]

WIKI

my $htmltext = wf ($wikitext);
like $htmltext, qr!'Ordinary'>extended link</a>!m,
     'extended links rendered correctly with default delimiters';
like $htmltext, qr!'http://nowhere\.com'>explicit URI</a>!m,
     'explicit URIs rendered correctly with default delimiters';
like $htmltext, qr!Usemod%20extended%20link'>Usemod extended link</a>!m,
     'Wiki URIs rendered correctly with default delimiters';

# Redefine the delimiters to the same thing again.
my %tags = (
	extended_link_delimiters => qr/(\[(?:\[[^][]*\]|[^][]*)\])/,
);

$htmltext = wf ($wikitext, \%tags);
like $htmltext, qr!'Ordinary'>extended link</a>!m,
     'extended links rendered correctly with default delimiters';
like $htmltext, qr!'http://nowhere\.com'>explicit URI</a>!m,
     'explicit URIs rendered correctly with default delimiters';
like $htmltext, qr!Usemod%20extended%20link'>Usemod extended link</a>!m,
     'Wiki URIs rendered correctly with default delimiters';

# Redefine the delimiters to something different.
%tags = (
	#extended_link_delimiters => qr/(\[\[[^][]*\]\])/,
	extended_link_delimiters => [qw([ ])],
);

$htmltext = wf ($wikitext, \%tags);

unlike $htmltext, qr!'Ordinary'>extended link</a>!m,
       'extended links rendered correctly with default delimiters';
unlike $htmltext, qr!'http://nowhere\.com'>explicit URI</a>!m,
       'explicit URIs rendered correctly with default delimiters';
like $htmltext, qr!Usemod extended link</a>[^\]]!m,
     '...and new delimiters recognised';
