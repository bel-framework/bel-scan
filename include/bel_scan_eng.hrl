%% This header exists to be possible to do pattern
%% matching in modules that behaves as bel_scan_eng.

-record(marker, {
    id :: bel_scan_mark:id(),
    re :: bel_scan_mark:re()
}).

-record(engine, {
    module  :: module(),
    markers :: [bel_scan_mark:t()],
    state   :: bel_scan_eng:state()
}).
