-- | 

module AsyncCatching where


threadDelay 4000; return "done" `catch` \e -> threadDelay 3000; return "not done!"

threadDelay 1000; throwTo tid
-- Throw again! This should not be catched!
threadDelay 1000; throwTo tid
