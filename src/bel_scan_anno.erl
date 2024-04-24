-module(bel_scan_anno).

-export_type([ t/0, loc/0, text/0, meta/0 ]).

% -record(anno, {
    % init_loc :: loc(),
    % end_loc  :: loc(),
    % meta     :: term()
% }).
% -opaque t()  :: #anno{}.
-type t()    :: {loc(), loc(), text(), meta()}.
-type loc()  :: bel_scan_loc:t().
-type text() :: binary().
-type meta() :: term().
