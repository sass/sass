#!/bin/bash -e
# Copyright 2018 Google Inc. Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.

curl https://kolkrabbi.heroku.com/apps/98fc74a8-ff56-4a21-85f6-7a1fc8ba95c9/github/push \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/javascript, */*; q=0.01" \
  -H "Authorization: Bearer $HEROKU_TOKEN" \
  -H "accept-encoding: gzip, deflate, br" \
  -H "accept-language: en-US,en;q=0.9" \
  -H "origin: https://dashboard.heroku.com" \
  -H "referer: https://dashboard.heroku.com/" \
  -d '{"branch":"main"}' \
  --fail --output -
