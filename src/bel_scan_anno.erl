-module(bel_scan_anno).

-export_type([ t/0, loc/0, text/0, meta/0 ]).

-record(anno, {
    loc  :: loc(),
    text :: binary(),
    meta :: term()
}).
-opaque t()  :: #anno{}.
-type loc()  :: bel_loc:t().
-type text() :: binary().
-type meta() :: term().
