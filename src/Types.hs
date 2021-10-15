
data App = App
  { _heist :: Snaplet (Heist App)
  , _sess :: Snaplet SessionManager
  , _db :: Snaplet PersistState
  , _auth :: Snaplet (AuthManager App)
  , _service :: Snaplet S.Service
  }

