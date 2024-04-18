%% This header exists to be possible to do pattern
%% matching in modules that behaves as bel_scan_eng.

-record(marker, {
    id       :: bel_scan_mark:id(),
    re_start :: bel_scan_mark:re_start(),
    re_end   :: bel_scan_mark:re_end()
}).

-record(engine, {
    module  :: module(),
    markers :: [bel_scan_mark:t()],
    state   :: bel_scan_eng:state()
}).
