# Chess-backend

## Overview

This is the backend that powers the chess analytics web tool. Fundamentally, the goal of the backend is to provide a JSON Api that's consumed by the frontend.

The backend consists of two parts:
- An application to parse external chess databases into the backend database
- A web server that provides Api endpoints

## Parsing external data

The external data for chess games comes in through a list of text files in Pgn format. A Pgn file simply describes the moves in a game. The main goal is to put this data into a SQL database is to allow a variety of queries. The philosophy is that this step should do as much of the hard work as possible, so that the resulting data is not corrupt and easy to query.

This library provides the `fill_db` application which takes those text files, parses them into games, and stores the relevant information in the database. The information in a Pgn file is unstructured. For instance, a player is referred to by their name "Carlsen, Magnus". The goal is to turn this into structured, queryable data, which includes the following:
- `Player` with a first and last name
- `Game`, which requires a player with the white and black pieces and a result. The `Game` includes a Pgn of the moves, and the moves are checked to parse into a game.
- A `MoveEval`, which provides an evaluation of every single position in the game. This data can be used to calculate how closely a player's moves match the recommendations by the computer.

## Providing Api endpoints

The Api is provided by Servant. The beauty of Servant is that the Api is fundamentally defined by it's (quite readable) type. The endpoints correspond to data pulls, for instance there is one data pull to obtain all the tournaments in the database, and another one to provide average evaluations by mover number.

When the resulting data pull is straightforward, for instance to get the list of tournaments, the Api is a wrapper on top of a SQL query, which I try to do using Esqueleto. Other Api endpoints, e.g. the move number evaluations, require more complicated data transformations that are done in Haskell proper. 

## Technology

This backend is written in Haskell.

I use Snap to provide the web server, and Servant to provide the Api inside the web server. For now, using Snap is overkill, but it will come in handy when allowing for user accounts and permissions.

I use a postgres database, which I access using Persistent and Esqueleto.



