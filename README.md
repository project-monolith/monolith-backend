# monolith-backend

[![Build Status](https://travis-ci.org/project-monolith/monolith-backend.svg?branch=master)](https://travis-ci.org/project-monolith/monolith-backend)

This is the backend service for Project Monolith. It is responsible for
fetching data from various sources, aggregating them, and providing them to
clients (such as monolith-frontend) upon request. It should use static data
where possible and include a caching layer so that real-time data providers
don't get mad at us.

## Getting it, building and running it

You will need to get the latest ghc and cabal-install, both of which are 
available in homebrew. We are using ghc 7.10.1 and cabal 1.22.0.0, so make
sure that you have recently run `brew update` or are otherwise up to date
using your package manager of choice. Then, to build and run it in place:

    git clone git@github.com:project-monolith/monolith-backend.git
    cd monolith-backend
    cabal update
    cabal sandbox init
    cabal install -j --only-dependencies
    cabal build
    dist/build/monolith-backend/monolith-backend

Ask @mdunsmuir if you run into problems getting it to work, or you can check
out the .travis.yml file to see a slightly different build method that may
work better for you. If you are using
an operating system whose default Haskell packages are not up-to-date (like
Ubuntu, unfortunately), then you will need to find some other way of getting
ghc and cabal. The website for the former offers easy-to-install linux
binaries, and the github repo for the latter includes a build/install script.

Better build/deploy options coming soon. I am currently planning to distribute
it as a packaged binary for whatever Linux environment we end up running on.

## Contributing

The Monolith backend is written in [Haskell](https://www.haskell.org). Its
architecture is loosely based on the pattern described
[here](https://www.fpcomplete.com/user/meiersi/the-service-pattern), and it
currently uses the Scotty web framework.

Anyone can submit a pull request! Haskell style is difficult to pin down in
a rigid style guide, but your code should follow
[these guidelines](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).

## REST API Methods

In all these methods, `stop_id` should be a stop id of the form seen in GTFS
and One Bus Away data. These are generally something like agencyid_stopid,
e.g. `1_600`. You can find these stop IDs for particular stops by using the
One Bus Away app.

### GET /stops/{stop_id}/trips

Returns a JSON object containing information about incoming trips in the near
future. The trips are organized under objects representing the routes they are
servicing. The ordering of the routes is ascending by the lowest wait time of all
the trips for a route (though attributes are provided for the client to use a
different comparator).

#### Parameters

* **n_trips** (*optional*) The number of trips to return. Default: 9

#### Example

```json
{
    "stopId": "1_300",
    "stopTimestamp": 1429740287,
    "stopDesc": "bar",
    "stopRoutes": [{
        "earliestTrip": 1429740562,
        "routeId": "1_100068",
        "routeNumber": "177",
        "routeTrips": [{
            "tripArrival": 1429740562,
            "tripRouteId": "1_100068",
            "tripWaitTime": 5,
            "tripHeadSign": "FEDERAL WAY, 320TH P&R VIA I-5",
            "tripId": "1_28152633",
            "tripWaitSource": "Scheduled"
        }],
        "routeDesc": "Federal Way P&R Via I-5"
    }, {
        "earliestTrip": 1429740840,
        "routeId": "40_577",
        "routeNumber": "577",
        "routeTrips": [{
            "tripArrival": 1429740840,
            "tripRouteId": "40_577",
            "tripWaitTime": 9,
            "tripHeadSign": "Federal Way",
            "tripId": "40_5893734",
            "tripWaitSource": "Realtime"
        }, {
            "tripArrival": 1429741080,
            "tripRouteId": "40_577",
            "tripWaitTime": 13,
            "tripHeadSign": "Federal Way",
            "tripId": "40_5893757",
            "tripWaitSource": "Realtime"
        }],
        "routeDesc": ""
    }]
}
```
### GET /stops/{stop_id}/ticker

Returns a (JSON) string of ticker-styled text, skipping some number of
upcoming routes. This method is complementary to the `trips` method, and
is used to allow compact display of routes that don't fit on the primary
display.

#### Parameters

* **n_skip_trips** (*optional*) The number of imminent trips to leave out of the ticker. Default: 9

#### Example

```json
"Route 24 in 2, 29 mins. Route D Line in 2, 15, 19, 32 mins. Route 16 in 4, 23 mins. Route 5 in 6, 20 mins. Route 13 in 9, 26, 33 mins. Route 26 in 13, 35 mins. Route 18 in 32 mins."
```

## Code Documentation

Automatically generated documentation, hopefully updated as of the last significant change, can be found here:

http://project-monolith.github.io/monolith-backend
