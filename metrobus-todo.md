## ScRipts
### schedule-updateR
- [x] ~~create 'short_description' col <-remove all text after last \\. in bus_inicidents table~~
- [x] ~~change nrow for bus_incidents$routeid to unique(bus_incidents$routeid)~~
- [x] remove line that deletes file folders because, bestie :), that is deleting the files that the schedule-saveR is using :) :) :)

### schedule-saveR
- [x] ~~change cron schedule to 3 or 4 am/pm~~
  - [X] ~~i think this should either update more frequently or the earlier run should happen at 5 or 6 am.~~
        - switched this so the script runs every four hours instead of twice a day.
- [x] ~~add bus positions to script (might have done this already)~~
- [x] ~~make sure that scripts with errors are actually getting added to the error folder~~
    - [ ] make it so that all scripts and errors are going into the log folder instead of splitting things between the log/routes folder

### bus-position-saveR
- [ ] fix time columns
- [x] ~~something is wrong with how often this is refreshing~~

## geneRal pRoject
- [x] ~~where is the S2??? WHERE is the S2?!?!?!?! WHERE!!!! IS???? THE?!?!?!?! S2!!!!!!! my biggest op fr.~~
      - I think this is fixed now.
- [x] ~~table that logs unique vehicle ids + ability to indicate whether a specific vehicle is an extendo or not~~
- [ ] diversify api keys
### file/memoRy management
- [x] ~~create specific log file folder~~
- [ ] delete db log records older than 3 days old once a day
- [ ] delete old logs/date folders automatically
    - [ ] i think we should do away with the date folders completely but this may be the easier solution for now

### eRRoR tRacking
- [x] ~~if a script generates 0 rows, move it to an error file~~

