# monolith-backend

This is the backend service for Project Monolith. It is responsible for
fetching data from various sources, aggregating them, and providing them to
clients (such as monolith-frontend) upon request. It should use static data
where possible and include a caching layer so that real-time data providers
don't get mad at us.

## Getting it, building and running it

You will need to get the latest ghc and cabal-install, both of which are 
available in homebrew. Then, to build and run it in place:

    git clone git@github.com:project-monolith/monolith-backend.git
    cd monolith-backend
    cabal sandbox init
    cabal install --only-dependencies
    cabal build
    dist/build/monolith-backend/monolith-backend

Ask @mdunsmuir if you run into problems getting it to work. If you are using
an operating system whose default Haskell packages are not up-to-date (like
Ubuntu, unfortunately), then you will need to find some other way of getting
ghc and cabal. The website for the former offers easy-to-install linux
binaries, and the github repo for the latter includes a build/install script.

Better build/deploy options coming soon.

## Contributing

The Monolith backend is written in [Haskell](https://www.haskell.org). Its
architecture is loosely based on the pattern described
[here](https://www.fpcomplete.com/user/meiersi/the-service-pattern), and it
currently uses the Scotty web framework.

Anyone can submit a pull request! Haskell style is difficult to pin down in
a rigid style guide, but your code should follow
[these guidelines](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).

## Documentation

### REST API Methods

Coming soon!

### Code Docs

Automatically generated documentation, hopefully updated as of the last significant change, can be found here:

http://project-monolith.github.io/monolith-backend
