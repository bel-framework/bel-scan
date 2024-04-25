-module(bel_scan_anno).

-export_type([ t/0, loc/0, meta/0 ]).

% -record(anno, {
    % init_loc :: loc(),
    % end_loc  :: loc(),
    % meta     :: term()
% }).
% -type t()  :: #anno{}.
-type t()    :: {{loc(), loc()}, meta()}.
-type loc()  :: {non_neg_integer(), non_neg_integer()}.
-type meta() :: term().
