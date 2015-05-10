# monolith-backend

This is the backend service for Project Monolith. It is responsible for
fetching data from various sources, aggregating them, and providing them to
clients (such as monolith-frontend) upon request. It should use static data
where possible and include a caching layer so that real-time data providers
don't get mad at us.

## Getting it, building and running it

### ghc and cabal-install

You will need `ghc` and `cabal-install` to build the monolith
backend. On Mac OS X Yosemite, both these things can be obtained very easily
with the following command, assuming you have homebrew installed:

    brew install ghc cabal-install

As newer versions of `ghc` (the Haskell compiler) and `cabal-install` (a tool
for installing and building packages) become available, they will probably
work just fine with the monolith backend until the next big change in the same
vein as the applicative-monad proposal in 7.10. The versions in use as of this
writing were `ghc` 7.10.1 and `cabal-install` 1.22.0.0.

If you are using
an operating system whose default Haskell packages are not up-to-date (like
Ubuntu, unfortunately), then you will need to find some other way of getting
`ghc` and `cabal-install`. The website for the former offers easy-to-install linux
binaries, and the github repo for the latter includes a build/install script.

### monolith-backend

Once you have `ghc` and `cabal-install`, getting the monolith backend and
building it is easy:

    git clone git@github.com:project-monolith/monolith-backend.git
    cd monolith-backend
    cabal update
    cabal sandbox init
    cabal install -j --only-dependencies
    cabal build
    dist/build/monolith-backend/monolith-backend

Ask @mdunsmuir if you run into problems getting it to work, or you can check
out the .travis.yml file to see a slightly different build method that may
work better for you.

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
    "routes": [{
        "trips": [{
            "waitSource": "Scheduled",
            "routeId": "1_100256",
            "id": "1_26432294",
            "arrival": 1431302070,
            "headSign": "NORTHGATE EASTLAKE",
            "waitTime": "DUE"
        }, {
            "waitSource": "Realtime",
            "routeId": "1_100256",
            "id": "1_26432295",
            "arrival": 1431303870,
            "headSign": "NORTHGATE EASTLAKE",
            "waitTime": 30
        }],
        "id": "1_100256",
        "number": "66",
        "desc": "Northgate TC to U-District to Downtown Seattle"
    }, {
        "trips": [{
            "waitSource": "Realtime",
            "routeId": "1_100210",
            "id": "1_21672722",
            "arrival": 1431302364,
            "headSign": "DOWNTOWN SEATTLE N BEACON HILL",
            "waitTime": 4
        }, {
            "waitSource": "Realtime",
            "routeId": "1_100210",
            "id": "1_21672723",
            "arrival": 1431302703,
            "headSign": "DOWNTOWN SEATTLE N BEACON HILL",
            "waitTime": 10
        }, {
            "waitSource": "Realtime",
            "routeId": "1_100210",
            "id": "1_21672724",
            "arrival": 1431303563,
            "headSign": "DOWNTOWN SEATTLE N BEACON HILL",
            "waitTime": 24
        }],
        "id": "1_100210",
        "number": "36",
        "desc": "Othello Stn - Beacon Hill - Downtown"
    }, {
        "trips": [{
            "waitSource": "Realtime",
            "routeId": "1_100219",
            "id": "1_18921744",
            "arrival": 1431302386,
            "headSign": "E QUEEN ANNE SEATTLE CENTER E",
            "waitTime": 5
        }],
        "id": "1_100219",
        "number": "4",
        "desc": "E QA - Downtown - Judkins Pk"
    }, {
        "trips": [{
            "waitSource": "Realtime",
            "routeId": "1_100089",
            "id": "1_25931793",
            "arrival": 1431302633,
            "headSign": "W QUEEN ANNE SEATTLE CENTER W",
            "waitTime": 9
        }],
        "id": "1_100089",
        "number": "2",
        "desc": "W QA - Downtown - Madrona Pk"
    }, {
        "trips": [{
            "waitSource": "Realtime",
            "routeId": "1_102581",
            "id": "1_24666412",
            "arrival": 1431302722,
            "headSign": "BALLARD UPTOWN",
            "waitTime": 10
        }, {
            "waitSource": "Realtime",
            "routeId": "1_102581",
            "id": "1_24666413",
            "arrival": 1431303500,
            "headSign": "BALLARD UPTOWN",
            "waitTime": 23
        }],
        "id": "1_102581",
        "number": "D Line",
        "desc": "Seattle Blue Ridge to Crown Hill to Ballard to Sea"
    }, {
        "trips": [{
            "waitSource": "Scheduled",
            "routeId": "1_102615",
            "id": "1_25513812",
            "arrival": 1431302768,
            "headSign": "AURORA VILLAGE TRANSIT CENTER",
            "waitTime": 11
        }, {
            "waitSource": "Scheduled",
            "routeId": "1_102615",
            "id": "1_25513832",
            "arrival": 1431303668,
            "headSign": "AURORA VILLAGE TRANSIT CENTER",
            "waitTime": 26
        }],
        "id": "1_102615",
        "number": "E Line",
        "desc": "Aurora Village to Downtown Seattle"
    }],
    "id": "1_578",
    "timestamp": 1431302097,
    "desc": "3rd Ave & Pike St"
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

### GET /stops/{stop_id}/vicinity

Returns data concerning nearby bus stops, bike shares, car shares, and so on.
Intended for use by the map view in the front end.

#### Parameters

* **radius** (*optional*) The radius, in meters, to search within for "nearby attractions". Default: 100

#### Example
```json
{
    "radius": 100,
    "bikeShares": [],
    "carShares": [],
    "homeStop": {
        "routes": [
            "1",
            "2",
            "3",
            "4",
            "7",
            "13",
            "14",
            "15",
            "29",
            "36",
            "49",
            "66",
            "70",
            "82",
            "83",
            "C Line",
            "D Line",
            "E Line"
        ],
        "distanceFromHomeStop": null,
        "location": {
            "lat": 47.61029,
            "lon": -122.338173
        },
        "direction": "NW",
        "name": "3rd Ave & Pike St",
        "id": "1_578"
    },
    "events": [],
    "nearbyStops": [{
        "routes": [
            "5",
            "15",
            "21",
            "24",
            "26",
            "28",
            "55",
            "56",
            "57",
            "116",
            "118",
            "119",
            "120",
            "124",
            "125",
            "131",
            "132",
            "304",
            "355",
            "C Line",
            "D Line",
            "E Line"
        ],
        "distanceFromHomeStop": 57.72667214405003,
        "location": {
            "lat": 47.609791,
            "lon": -122.337959
        },
        "direction": "SE",
        "name": "3rd Ave & Pike St",
        "id": "1_431"
    }, {
        "routes": [
            "7",
            "11",
            "43",
            "49",
            "84"
        ],
        "distanceFromHomeStop": 67.86318808561133,
        "location": {
            "lat": 47.610844,
            "lon": -122.338554
        },
        "direction": "W",
        "name": "Pine Street Island & 3rd Ave",
        "id": "1_1111"
    }, {
        "routes": [
            "10",
            "11",
            "43",
            "49",
            "143",
            "157",
            "158",
            "159",
            "179",
            "192",
            "577"
        ],
        "distanceFromHomeStop": 91.00187462586949,
        "location": {
            "lat": 47.611103,
            "lon": -122.338028
        },
        "direction": "W",
        "name": "Pine St & 4th Ave",
        "id": "1_1120"
    }, {
        "routes": [
            "590",
            "592",
            "594",
            "595"
        ],
        "distanceFromHomeStop": 92.02745562060316,
        "location": {
            "lat": 47.610791,
            "lon": -122.337195
        },
        "direction": "NW",
        "name": "4th / Pike - drop off only",
        "id": "3_13217"
    }, {
        "routes": [
            "1",
            "2",
            "3",
            "4",
            "7",
            "13",
            "14",
            "16",
            "17",
            "18",
            "25",
            "27",
            "33",
            "36",
            "40",
            "66",
            "70",
            "82",
            "83",
            "84"
        ],
        "distanceFromHomeStop": 99.661037403426,
        "location": {
            "lat": 47.610973,
            "lon": -122.339035
        },
        "direction": "SE",
        "name": "3rd Ave & Pine St",
        "id": "1_430"
    }, {
        "routes": [
            "402",
            "405",
            "410",
            "412",
            "413",
            "415",
            "416",
            "417",
            "421",
            "422",
            "424",
            "425",
            "435",
            "510",
            "511",
            "512",
            "513"
        ],
        "distanceFromHomeStop": 100.13199967637266,
        "location": {
            "lat": 47.610948,
            "lon": -122.33726
        },
        "direction": "NW",
        "name": "4th Ave and Pike St",
        "id": "29_2976"
    }, {
        "routes": [
            "41",
            "64",
            "71",
            "72",
            "73",
            "106",
            "111",
            "114",
            "143",
            "150",
            "177",
            "178",
            "190",
            "212",
            "212",
            "214",
            "217",
            "252",
            "257",
            "268",
            "311",
            "545",
            "550",
            "554"
        ],
        "distanceFromHomeStop": 100.80691210829458,
        "location": {
            "lat": 47.61095,
            "lon": -122.33725
        },
        "direction": "NW",
        "name": "4th Ave & Pike St",
        "id": "1_700"
    }, {
        "routes": [
            "5",
            "16",
            "17",
            "18",
            "21",
            "24",
            "25",
            "26",
            "28",
            "33",
            "37",
            "40",
            "55",
            "56",
            "57",
            "116",
            "118",
            "119",
            "120",
            "124",
            "304",
            "355",
            "994"
        ],
        "distanceFromHomeStop": 110.71593996723283,
        "location": {
            "lat": 47.611137,
            "lon": -122.338951
        },
        "direction": "NW",
        "name": "3rd Ave & Pine St",
        "id": "1_590"
    }]
}
```

## Code Documentation

Automatically generated documentation, hopefully updated as of the last significant change, can be found here:

http://project-monolith.github.io/monolith-backend
