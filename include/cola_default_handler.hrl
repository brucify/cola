-record(state,  { controller_module    :: atom()
                , allowed_methods      :: [atom()]
                , request_params = []  :: [{atom(), any()}]
                , request_body         :: binary()
                , client               :: atom()
                }).