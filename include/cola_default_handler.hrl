-record(state,  { controller_module    :: atom()
                , allowed_methods      :: [atom()]
                , request_params = []  :: [{atom(), any()}]
                , request_body         :: binary() | undefined
                , raw_request_body     :: binary() | undefined
                , client               :: atom()
                }).