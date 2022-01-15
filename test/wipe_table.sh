#!/bin/bash

psql -U postgres -d chess < wipe_table.sql
