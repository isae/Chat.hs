# Husky
Simple client-server chat with GUI and database implemented on Haskell (as university's FP course work). Actually, looks like it is some hybrid of chat and text messenger :)

Used packages:
### Server
- [scotty] (https://hackage.haskell.org/package/scotty) for simple requests routing
- [persistent] (https://hackage.haskell.org/package/persistent) with SQLite for persistence and object-relational mapping
- [esqueleto] (https://hackage.haskell.org/package/esqueleto) for complex SQL queries 
- [aeson] (https://hackage.haskell.org/package/aeson) for JSON parsing

### Client
- [gtk3] (https://hackage.haskell.org/package/gtk3) for GUI
- [http-conduit] (https://hackage.haskell.org/package/http-conduit) for sending request to server

