module Main (main) where

import Core (runParser)
import Hson (parseJsonValue)

jsonString :: String
jsonString =
    "{\
    \  \"company\": {\
    \    \"name\": \"TechNova Solutions\",\
    \    \"founded\": 2010,\
    \    \"departments\": [\
    \      {\
    \        \"name\": \"Engineering\",\
    \        \"employees\": [\
    \          {\
    \            \"id\": 101,\
    \            \"name\": \"Alice Smith\",\
    \            \"skills\": [\"Haskell\", \"Scala\", \"Kubernetes\"],\
    \            \"projects\": [\
    \              {\"name\": \"CompilerX\", \"status\": \"active\", \"budget\": 120000},\
    \              {\"name\": \"LambdaCloud\", \"status\": \"maintenance\", \"budget\": 50000}\
    \            ],\
    \            \"manager\": null\
    \          },\
    \          {\
    \            \"id\": 102,\
    \            \"name\": \"Bob Lee\",\
    \            \"skills\": [\"Rust\", \"Go\", \"Docker\"],\
    \            \"projects\": [],\
    \            \"manager\": 101\
    \          }\
    \        ]\
    \      },\
    \      {\
    \        \"name\": \"Research\",\
    \        \"employees\": [\
    \          {\
    \            \"id\": 201,\
    \            \"name\": \"Carol White\",\
    \            \"skills\": [\"AI\", \"ML\", \"Python\", \"Julia\"],\
    \            \"projects\": [\
    \              {\"name\": \"VisionAI\", \"status\": \"prototype\", \"budget\": 200000}\
    \            ],\
    \            \"manager\": null\
    \          }\
    \        ]\
    \      },\
    \      {\
    \        \"name\": \"HR\",\
    \        \"employees\": []\
    \      }\
    \    ],\
    \    \"offices\": [\
    \      {\
    \        \"city\": \"Berlin\",\
    \        \"address\": \"Alexanderplatz 1\",\
    \        \"phones\": [\"+49-30-123456\", \"+49-30-654321\"]\
    \      },\
    \      {\
    \        \"city\": \"Munich\",\
    \        \"address\": \"Marienplatz 10\",\
    \        \"phones\": []\
    \      }\
    \    ],\
    \    \"remote\": true,\
    \    \"annualRevenue\": 3500000\
    \  },\
    \  \"products\": [\
    \    {\
    \      \"id\": \"P-001\",\
    \      \"name\": \"NovaDB\",\
    \      \"released\": 2018,\
    \      \"tags\": [\"database\", \"distributed\", \"cloud\"],\
    \      \"supported\": true,\
    \      \"downloads\": 1200000\
    \    },\
    \    {\
    \      \"id\": \"P-002\",\
    \      \"name\": \"StreamFlow\",\
    \      \"released\": 2021,\
    \      \"tags\": [\"streaming\", \"analytics\"],\
    \      \"supported\": false,\
    \      \"downloads\": 340000\
    \    },\
    \    {\
    \      \"id\": \"P-003\",\
    \      \"name\": \"InsightAI\",\
    \      \"released\": 2024,\
    \      \"tags\": [\"ai\", \"ml\", \"vision\"],\
    \      \"supported\": true,\
    \      \"downloads\": 25000\
    \    }\
    \  ],\
    \  \"events\": [\
    \    {\
    \      \"type\": \"webinar\",\
    \      \"title\": \"Functional Programming in Production\",\
    \      \"date\": \"2025-06-15\",\
    \      \"participants\": [101, 201, 102]\
    \    },\
    \    {\
    \      \"type\": \"hackathon\",\
    \      \"title\": \"AI for Good\",\
    \      \"date\": \"2025-07-01\",\
    \      \"participants\": [201, 101]\
    \    }\
    \  ],\
    \  \"settings\": {\
    \    \"features\": {\
    \      \"beta\": [\"dark_mode\", \"live_collab\"],\
    \      \"stable\": [\"export\", \"import\", \"notifications\"]\
    \    },\
    \    \"limits\": {\
    \      \"maxUsers\": 1000,\
    \      \"maxProjects\": 100\
    \    },\
    \    \"maintenance\": false\
    \  },\
    \  \"history\": [\
    \    {\"year\": 2010, \"event\": \"Founded\"},\
    \    {\"year\": 2012, \"event\": \"First Product Launched\"},\
    \    {\"year\": 2018, \"event\": \"Opened Berlin Office\"},\
    \    {\"year\": 2021, \"event\": \"Reached 1M Downloads\"},\
    \    {\"year\": 2024, \"event\": \"AI Division Established\"}\
    \  ]\
    \}"

otherString =
    "{\
    \    \"id\": 101,\
    \    \"name\": \"Alice Smith\",\
    \    \"skills\": [\"Haskell\", \"Scala\", \"Kubernetes\"],\
    \    \"projects\": [\
    \      {      \"name\": \"CompilerX\", \"status\": \"active\", \"budget\": 120000},\
    \      {\"name\": \"LambdaCloud\", \"status\": \"maintenance\", \"budget\": 50000}\
    \    ],\
    \    \"manager\": null,\
    \    \"projects\": [\
    \      {\"name\": \"CompilerX\", \"status\": \"active\", \"budget\": 120000},\
    \      {\"name\": \"LambdaCloud\", \"status\": \"maintenance\", \"budget\": 50000}\
    \    ]\
    \}"

myString =
    "{\
    \  \"company\": {\
    \    \"name\": \"TechNova Solutions\",\
    \    \"founded\": 2010,\
    \    \"departments\": [\
    \      {\
    \        \"name\": \"Engineering\"\
    \      }\
    \    ]\
    \  }\
    \}"
main :: IO ()
main = do
    let parsed = runParser parseJsonValue myString
    -- let parsed = runParser parseJsonValue "{ \"a\" :   { \"b\"   : { \"c\" : 1}} }"
    print parsed
