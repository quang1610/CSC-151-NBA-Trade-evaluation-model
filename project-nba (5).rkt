#lang racket
(require csc151)
;;; Project title: NBA TRADE ANALYSIS
;;; Authors: JAMES LIM, NEEL "Hates the Celtics" Lal, QUANG "Hates the Denver" NGUYEN.
;;; Project's purpose:
;;; Database: "2017-2018 statistic.csv"
;;;  - This database contains records of players in every game in NBA 2017- 2018 season. Each record is a player-record (format is defined below), contains his statistic
;;;       in 1 game.

#|
For the sake of consistency, we will define
2 types of record:
- a player-record: is a list contains
   "player's displayed Name"-"team name abbreviation"-"player's time"-"player's points"-"player's assists"-"player's rebound"-"player's blocks"-
       "player's steals"-"player's Turn Over"-"player's FGA"-"player's FGM"-"player's Height"-"player's position".


- a team-record: is a list contains
   "team name abbreviation"-"current players' total play time"-"total players' points"-"total players' assists"-"total players' rebound"-
       "total players' blocks"-"total players' steals"-"total players' Turn Over"-"total players' FGA"-"total players' FGM".

- when we mention just "record": it can mean team-record or player-record.

- an entry is an element in a record such as "player's displayed Name", "player's points", "total players' steals"...

- database is a list contains records.
|#

#|
!!! PRE-CAUTION, IMPORTANT !!!
The database has the data of entire 2017-2018 season. There are trading season in the middle of season, thus some players might have their game player-records of
   2 different teams.
ie: Blake Griffin played for the LAC before he went to DET.
We would count his stat of the entire season for his latest team, which is DET.
|#

;read the csv "2017-2018 statistic.csv" file into players-all-games-database list (remove the title line)
(define players-all-games-database (cdr (read-csv-file "2017-2018 statistic.csv")))
;(define val players-all-games-database)

; Players-and-numb-of-games is the list contains
;    pairs ("players' name" . "the number of games they play in the entire 2017-2018 season")
; From this list, we gain 2 things:
;    1. easy way to achieve a list of just players' name. (using map car players-and-numb-of-games)
;    2. number of game each players' play 
(define players-and-numb-of-games
  (tally-all (map car players-all-games-database)))
;> (take players-and-numb-of-games 3)
;'(("Álex Abrines" 75)
;  ("Aaron Brooks" 32)
;  ("Aaron Gordon" 58))
;  ie Aaron Brooks plays 32 games in 2017-2018 season.

;;; Procedure:
;;;  add-up-1-entry
;;; Parameters:
;;;  order, the index of entry in the record we want to add up the value.
;;;  database, a list of records.
;;; Purpose:
;;;  calculate the sum of all the entries with index "order" in every record in database  
;;; Produce:
;;;  result, a number.
;;; Pre-conditions:
;;;  data type of entry with index "order" must be number.
;;;  order must be within the length of record in database
;;; Post-conditions:
;;;  the datatype of entry is maintained.
(define add-up-1-entry
  (lambda (order database)
    (reduce + (map (section list-ref <> order) database))))
;> (add-up-1-entry 1 (list (list 1 2 3) (list 4 5 6)))
;7
;> (add-up-1-entry 0 (list (list 1 2 3) (list 4 5 6)))
;5








;;;This section of code deal with players' stats 
;PLAYERS-STATISTICS
;;; Procedure:
;;;  player-is?
;;; Parameters:
;;;  name, a string - name of player
;;;  player-record, a list in format of player-record.
;;; Purpose:
;;;  check if player-record belong to player named name?
;;; Produce:
;;;  result, a boolean value, either true or false.
;;; Pre-conditions:
;;;  [no additional cond]
;;; Post-condition:
;;;  [no additional cond]
(define player-is?
  (lambda (name player-record)
    (string-ci=? name (car player-record))))

;;; Produce:
;;;  get-records-of-one-player
;;; Parameters:
;;;  name, a string - name of a player.
;;;  database, a table contains player-records.
;;; Purpose:
;;;  return a list of player-records belong to player named name.
;;; Produce:
;;;  result, a database of player-records.
;;; Pre-conditions:
;;;  [no additional]
;;; Post-conditions:
;;;  if there is no player named name in database, result is NULL
;;;  the number of records in result is = number of records in database with player name.
(define get-records-of-one-player
  (lambda (name database)
    (filter (section player-is? name <>) database)))
;> (take (get-records-of-one-player "Stephen Curry" players-all-games-database) 15)
;'(("Stephen Curry" "GS" 30 22 4 5 0 1 2 18 8 75 "PG\r")
;  ("Stephen Curry" "GS" 35 28 8 3 0 0 1 16 7 75 "PG\r")
;  ("Stephen Curry" "GS" 30 37 3 6 0 0 2 17 9 75 "PG\r")
;  ("Stephen Curry" "GS" 31 29 8 2 0 4 6 15 7 75 "PG\r")
;  ("Stephen Curry" "GS" 38 30 5 4 1 3 2 20 9 75 "PG\r")
;  ("Stephen Curry" "GS" 36 20 8 5 0 2 2 19 7 75 "PG\r")
;  ("Stephen Curry" "GS" 35 27 8 6 0 1 5 17 11 75 "PG\r")
;  ("Stephen Curry" "GS" 30 31 6 5 1 2 1 14 9 75 "PG\r")
;  ("Stephen Curry" "GS" 32 21 4 8 0 3 2 13 7 75 "PG\r")
;  ("Stephen Curry" "GS" 30 22 11 0 0 2 3 14 7 75 "PG\r")
;  ("Stephen Curry" "GS" 34 16 4 5 0 2 1 19 5 75 "PG\r")
;  ("Stephen Curry" "GS" 30 22 8 8 1 2 5 16 7 75 "PG\r")
;  ("Stephen Curry" "GS" 30 22 9 4 0 1 2 12 5 75 "PG\r")
;  ("Stephen Curry" "GS" 31 9 5 6 0 4 4 14 3 75 "PG\r")
;  ("Stephen Curry" "GS" 35 35 5 5 1 0 2 22 11 75 "PG\r"))

;;; Procedure:
;;;  add-up-player-statistics
;;; Parameters:
;;;  database, a list of player-records
;;; Purpose:
;;;  *database must contain only player-records of 1 player
;;;  output 1 record of that player with most of his entries added up to total (not his name, his team name, his height, his position).
;;;    (ie player's points in result would be total points in all records in database...)
;;; Produce:
;;;  result, a player-record of 1 player.
;;; Pre-condition:
;;;  database must contain only player-records of 1 player
;;;  database must not be NULL.
;;; Post-conditions:
;;;  the format of result is player-record.
;;; CAUTION:
;;;    for players who were traded mid-season (players that played for 2 or more teams in 2017 2018 season),
;;;       all their stat belong to their latest team. For example, Blake Griffin played for LAC then DET, his entire season
;;;       stats belong to DET
;;;       > (add-up-player-statistics (get-records-of-one-player "Blake Griffin" players-all-games-database))
;;;       '("Blake Griffin" "DET" 1969 1242 334 429 17 41 167 996 436 82 "PF\r")
(define add-up-player-statistics
  (lambda (database)
    (let ([header (take (list-ref database (- (length database) 1)) 2)]
          [ender (drop (car database) 11)])                            
      (append header
              (map (o (section add-up-1-entry <> database) (section + <> 2)) (iota 9))
              ender))))
;> (add-up-player-statistics (get-records-of-one-player "Stephen Curry" players-all-games-database))
;'("Stephen Curry" "GS" 1630 1346 309 261 8 80 153 864 428 75 "PG\r")

;;; Procedure:
;;;  all-players-statistics
;;; Parameters:
;;;  player-list, a list of smaller lists. 
;;;  database, a list of player-records. (ie players-all-games-database)
;;; Purpose:
;;;  output a result-database contains records of each player and their statistics sum up from database.
;;; Produce:
;;;  result, list of player-records.
;;; Pre-conditions:
;;;  player-list contains pairs whose first elements are players' names.
;;; Post-conditions:
;;;  length of result is equal to length of player-list.
;;;  for each player from player-list we have 1 record in result.
;;;  the order of player-records in result is the same as in player-list.
;;; CAUTION:
;;;    for players who were traded mid-season (players that played for 2 or more teams in 2017 2018 season),
;;;       all their stat belong to their latest team. For example, Blake Griffin played for LAC then DET, his entire season
;;;       stats belong to DET
;;;       > (add-up-player-statistics (get-records-of-one-player "Blake Griffin" players-all-games-database))
;;;       '("Blake Griffin" "DET" 1969 1242 334 429 17 41 167 996 436 82 "PF\r")
(define all-players-statistics
  (lambda (players-lst database)
    (map (o (section add-up-player-statistics <>)
            (section get-records-of-one-player <> database)
            car)
         players-lst)))
;> (take (all-players-statistics players-and-numb-of-games players-all-games-database) 10)
;'(("Álex Abrines" "OKC" 1134 353 28 113 8 38 25 291 115 78 "SG\r")
;  ("Aaron Brooks" "MIN" 189 75 20 17 0 6 11 69 28 72 "PG\r")
;  ("Aaron Gordon" "ORL" 1915 1022 136 458 45 58 107 866 375 81 "PF\r")
;  ("Aaron Harrison" "DAL" 235 60 11 24 2 9 3 69 19 78 "SG\r")
;  ("Aaron Jackson" "HOU" 35 8 1 3 0 0 1 9 3 76 "F\r")
;  ("Abdel Nader" "BOS" 522 146 26 71 10 15 34 149 50 78 "SF\r")
;  ("Adreian Payne" "ORL" 43 21 0 9 0 2 2 10 7 82 "PF\r")
;  ("Al Horford" "BOS" 2275 927 339 530 79 43 131 753 368 82 "C\r")
;  ("Al Jefferson" "IND" 480 252 30 143 23 16 21 208 111 82 "C\r")
;  ("Alan Williams" "PHO" 71 20 8 22 1 5 7 18 7 80 "PF\r"))

;;;pre-defined database with players-records and their statistics added up from the entire season. 
(define database-all-players-statistics-all-season
  (all-players-statistics players-and-numb-of-games players-all-games-database))










;;;This section of code deal with teams stats 
;TEAMS-STATISTICS
;;;pre-define list of pairs in format ("team-name" . "number of player in team")
;;; based on database-all-players-statistics-all-season defined at the end of previous section.
(define teams-and-numb-of-players
  (tally-all (map cadr database-all-players-statistics-all-season)))   ;defined at the end of previous section
;> (take teams-and-numb-of-players 3)
;'(("OKC" 17) ("MIN" 15) ("ORL" 18))
;ie MIN has 15 players at the end of 2017-2018 season.

;;; Procedure:
;;;  team-is?
;;; Parameters:
;;;  team-name, a string - name of team.
;;;  player-record, a list in format of player-record.
;;; Purpose:
;;;  check if the player-record belong to team named team-name?
;;; Produce:
;;;  result, a boolean value, either true or false.
;;; Pre conditions:
;;;  [no additional cond]
;;; Pros-conditions:
;;;  [no additional cond]
(define team-is?
  (lambda (team-name player-record)
    (string-ci=? team-name (cadr player-record))))

;;; Produce:
;;;  get-records-of-one-team
;;; Parameters:
;;;  team-name, a string - name of a team.
;;;  database, a list contains player-records.
;;; Purpose:
;;;  return a list of player-records belong to team named team-name.
;;; Produce:
;;;  result, a list of player-records.
;;; Pre-conditionals:
;;;  [no additional conds]
;;; Post-conditionals:
;;;  if team with team-name does not exist in database, the result is null.
;;;  the length of result = the number of records in database belong to team.
(define get-records-of-one-team
  (lambda (team-name database)
    (filter (section team-is? team-name <>) database)))
;> (get-records-of-one-team "GS" database-all-players-statistics-all-season)
;'(("Andre Iguodala" "GS" 1628 384 211 245 38 54 67 320 148 78 "SF\r")
;  ("Chris Boucher" "GS" 1 0 0 1 0 0 0 1 0 82 "PF\r")
;  ("Damian Jones" "GS" 89 25 2 14 3 1 4 22 11 83 "C\r")
;  ("David West" "GS" 1001 495 138 238 75 47 80 378 216 81 "PF\r")
;  ("Draymond Green" "GS" 2286 775 509 536 91 95 201 619 282 79 "PF\r")
;  ("JaVale McGee" "GS" 613 310 33 169 57 21 27 219 136 84 "C\r")
;  ("Jordan Bell" "GS" 811 262 102 205 57 36 51 185 116 81 "PF\r")
;  ("Kevin Durant" "GS" 2328 1792 366 465 119 50 208 1221 630 81 "SF\r")
;  ("Kevon Looney" "GS" 910 267 42 216 55 34 31 192 112 81 "SF\r")
;  ("Klay Thompson" "GS" 2500 1459 186 277 34 56 127 1178 574 79 "SG\r")
;  ("Nick Young" "GS" 1390 581 36 126 7 38 41 488 201 79 "SG\r")
;  ("Omri Casspi" "GS" 742 300 51 200 19 18 37 207 120 79 "SF\r")
;  ("Patrick McCaw" "GS" 962 229 81 83 11 43 41 226 92 79 "G\r")
;  ("Quinn Cook" "GS" 741 312 89 82 1 12 32 252 122 74 "PG\r")
;  ("Shaun Livingston" "GS" 1137 394 140 131 20 34 58 343 172 79 "PG\r")
;  ("Stephen Curry" "GS" 1630 1346 309 261 8 80 153 864 428 75 "PG\r")
;  ("Zaza Pachulia" "GS" 971 373 109 320 17 38 74 263 149 83 "C\r"))

;;; Procedure:
;;;  add-up-team-statistics
;;; Parameters:
;;;  database, a list of player-records.
;;; Purpose:
;;;  *database must contain only player-records of 1 team
;;;  output team-record of that team with all its players' statistic add up to total. 
;;; Produce:
;;;  result, a team-record.
;;; Pre-conditions:
;;;  database must contain only player-records of 1 team
;;; Post-conditions:
;;;  result must have the format of team-record.
(define add-up-team-statistics
  (lambda (database)
    (let ([header (drop (take (car database) 2) 1)])
      (append header
              (map (o (section add-up-1-entry <> database) (section + <> 2)) (iota 9))))))
;> (add-up-team-statistics (get-records-of-one-team "GS" database-all-players-statistics-all-season))
;'("GS" 19740 9304 2404 3569 612 657 1232 6978 3509)
; ie in 2017-2018 season, total points scored by all current players of Golden State Warrior is 9304

;;; Procedure:
;;;  all-teams-statistics
;;; Parameters:
;;;  teams-list, a list of pairs.
;;;  database, a list of player-records.
;;; Purpose:
;;;  output a list contains records of each team and their statistics sum up from database.
;;; Produce:
;;;  result, table of records.
;;; Pre-conditions:
;;;  team-list must contain pairs with team name is the first element of the pair.
;;; Post-conditions:
;;;  The length of result = the length of team-list.
;;;  The order of team-records in result is same as teams' names in teams-list.
(define all-teams-statistics
  (lambda (team-lst database)
    (map (o (section add-up-team-statistics <>)
            (section get-records-of-one-team <> database)
            car) team-lst)))

;;;pre-defined a table contains list of records with format
(define database-all-teams-statistics-all-season
  (all-teams-statistics teams-and-numb-of-players
                        database-all-players-statistics-all-season))
;> (take database-all-teams-statistics-all-season 5)
;'(("OKC" 20539 9042 1790 3786 421 789 1144 7393 3349)
;  ("MIN" 19826 9017 1880 3423 345 690 987 7073 3376)
;  ("ORL" 18650 7965 1659 3261 388 559 1035 6630 2965)
;  ("DAL" 20125 8407 1822 3425 305 549 951 7074 3158)
;  ("HOU" 20615 9542 1806 3730 419 711 1111 7182 3311))










;;; This section of code apply diluted algorithm to player's stat. 
#|
;;; The algorith: for example stat is the value of an entry of a player-record. 82 is the total number of games in 1 NBA season.
    diluted-stat = (stat + (number-of-game-player-played / 82)*stat) / 2
    precaution: stat must be of following categories:
    "player's points"-"player's assists"-"player's rebound"-"player's blocks"-"player's steals"-"player's Turn Over"-"player's FGA"-"player's FGM"
;;; Reason why we need to apply diluted algorithm:

;;;
|#
;;; Procedure:
;;;   get-numb-of-games-of-players
;;; Parameters:
;;;   name, a string. player's name.
;;; Purpose:
;;;   return the number of games that player played in 2017-2018 season.
;;;      This proc used pre-defined database players-and-numb-of-games.
;;; Produce:
;;;   result a number (integer).
;;; Pre-conditions:
;;;   player-name must be spelled correctly, with correct capitalization.
;;;   player-name must be of a current NBA player in 2017-2018 season.
;;; Post-condition:
;;;   throw error if cannot find player named name.
;;;   result <= 82.
;;;   there are no players with same name.
(define get-numb-of-games-of-players
  (lambda (player-name)
    (cadr (assoc player-name players-and-numb-of-games))))
;> (get-numb-of-games-of-players "Stephen Curry")
;51
;ie Stephen Curry played 51 games in last season.

;;; Procedure:
;;;   injury-diluted-statistics
;;; Parameters:
;;;   player-record, a list in player record format.
;;; Purpose:
;;;   * player-record must contain the stat add up from the entire season.
;;;   Apply diluted alg on the following entries of player-record: "player's points"-"player's assists"-"player's rebound"-"player's blocks"-
;;;       "player's steals"-"player's Turn Over"-"player's FGA"-"player's FGM"
;;; Produce:
;;;   result, a player-record with diluted stats.
;;; Precondition:
;;;   player-record must contain the stat add up from the entire season.
;;; Post-conditions:
;;;   player's name, player's team, player's play time, player's height, and player's position is remained the same.
;;;   the order of entries is maintained.
(define injury-diluted-statistics
  (lambda (player-record)
    (let* ([player-name (car player-record)]
           [player-numb-of-games (get-numb-of-games-of-players player-name)]
           [header (take player-record 3)]
           [ender (drop player-record 11)])
      (append  header
               (map (section injury-diluted-statistics-helper <> player-numb-of-games)
                    (take (drop player-record 3) 8))
               ender))))
; For example we have Stephen Curry's record:
;    '("Stephen Curry" "GS" 1630 1346 309 261 8 80 153 864 428 75 "PG\r")
; his diluted player-record of the entire season:
;    > (injury-diluted-statistics (list "Stephen Curry" "GS" 1630 1346 309 261 8 80 153 864 428 75 "PG\r"))
;    '("Stephen Curry" "GS" 1630 1755 403 340 10 104 200 1127 558 75 "PG\r")

;;; Procedure:
;;;   injury-diluted-statistics-helper
;;; Parameters:
;;;   stat, an integer.
;;;   player-numb-of-games, an integer.
;;; Purpose:
;;;   Apply the diluted algorithm on stat, using player-numb-of-games.
;;; Produce:
;;;   result, a number.
(define injury-diluted-statistics-helper
  (lambda (stat player-numb-of-games)
    (round (/ (+ stat
                 (* (/ 82 player-numb-of-games)
                    stat))
              2))))

;;; Procedure:
;;;   all-players-statistics-diluted
;;; Parameters:
;;;   database: a list contains player-records.
;;; Purpose:
;;;   Apply injury-diluted-statistics for each player-record in database.
;;; Produce:
;;;   result, database of plyer-records with diluted stats.
;;; Pre-conditions:
;;;   [no additional conds]
;;; Post-conditions:
;;;   the length of result = the length of database.
;;;   the order of player-records in database is the same in result.
(define all-players-statistics-diluted
  (lambda (database)
    (map injury-diluted-statistics database)))

;;; pre-defined database of diluted players' stats of the entire season:
(define database-all-players-statistics-all-season-diluted
  (all-players-statistics-diluted database-all-players-statistics-all-season))
;> (take database-all-players-statistics-all-season-diluted 5)
;'(("Álex Abrines" "OKC" 1134 369 29 118 8 40 26 305 120 78 "SG\r")
;  ("Aaron Brooks" "MIN" 189 134 36 30 0 11 20 123 50 72 "PG\r")
;  ("Aaron Gordon" "ORL" 1915 1233 164 553 54 70 129 1045 453 81 "PF\r")
;  ("Aaron Harrison" "DAL" 235 303 56 121 10 46 15 349 96 78 "SG\r")
;  ("Aaron Jackson" "HOU" 35 332 42 124 0 0 42 374 124 76 "F\r"))













;;;Expected stat for next season
;;; define the total play time of all players in 1 team. 5 players on the field, 48 min per game, 82 games.
(define total-play-time-of-all-players (* 5 (* 48 82)))

#|
This section of code apply prediction algorithm to team's stat.
The algorith: for example stat is the value of an entry of a player-record. 82 is the total number of games in 1 NBA season.
   predicted-stat = (diluted-team-stat / total team's play time) *  total-play-time-of-all-players
precaution: diluted-team-stat must be of following categories:
  "player's points"-"player's assists"-"player's rebound"-"player's blocks"-
      "player's steals"-"player's Turn Over"-"player's FGA"-"player's FGM"
Reason why we need to apply diluted algorithm:


|#

;;; Procedure:
;;;   predict-stat-next-season
;;; Parameters:
;;;   team-record, a list in format of team-record.
;;; Purpose:
;;;   apply predicting alg to team stats of the following categories:
;;;      "total players' points"-"total players' assists"-"total players' rebound"-
;;;       "total players' blocks"-"total players' steals"-"total players' Turn Over"-"total players' FGA"-"total players' FGM".
;;; Produce:
;;;   result, team-record with updated record.
;;; Pre-conditions:
;;;   no additional.
;;; Post-conditions:
;;;   in result, team's name and team total's play time is not changed.
(define predict-stat-next-season
  (lambda (team-record)
    (let ([total-team-play-time (cadr team-record)]
          [header (take team-record 2)])
      (append header
              (map (section predict-stat-next-season-helper <> total-team-play-time)
                   (drop team-record 2))))))
;For example we have stats of "NY" in 2017-2108 season:
; ("NY" 19034 8376 1936 3489 403 549 1164 7058 3248)
;Predict stat for the next season would be:
;> (predict-stat-next-season  (list "NY" 19034 8376 1936 3489 403 549 1164 7058 3248))
;'("NY" 19034 8660 2002 3607 417 568 1204 7298 3358)

;;; Procedure:
;;;   predict-stat-next-season-helper
;;; Parameters:
;;;   stat, a number
;;;   total-team-play-time, a number
;;; Purpose:
;;;   apply predicting agl to stat.
;;; Produce:
;;;   result, a number.
(define predict-stat-next-season-helper
  (lambda (stat total-team-play-time)
    (round (* (/ stat total-team-play-time)
              total-play-time-of-all-players))))

;;; Procedure:
;;;   predict-all-teams-stats-next-season
;;; Parameters:
;;;   database, a list contains team-records.
;;; Purpose:
;;;   apply predict-stat-next-season for each team-record in database.
;;; Produce:
;;;   result, a database contains team-records.
;;; Pre-conditions:
;;;   [no additional conds]
;;; Post-conditions:
;;;   length of result = length of database.
;;;   the order of team-records in database is the same as in result.
(define predict-all-teams-stats-next-season
  (lambda (database)
    (map predict-stat-next-season database)))

;;; pre-defined database with predicted teams' stats for next season if they keep their current teams. 
(define database-all-teams-statistics-all-next-season
  (predict-all-teams-stats-next-season database-all-teams-statistics-all-season))
;> (take database-all-teams-statistics-all-next-season 5)
;'(("OKC" 20539 9267 1840 3968 440 814 1195 7585 3450)
;  ("MIN" 19826 9898 2082 3596 358 784 1074 7753 3704)
;  ("ORL" 18650 10657 2136 4389 529 760 1361 8955 3966)
;  ("DAL" 20125 10137 2128 4362 376 687 1186 8643 3793)
;  ("HOU" 20615 11274 2114 4532 491 810 1443 8824 3959))











;;;Trade
;;; Procedure:
;;;   update-team-stat
;;; Parameters:
;;;   team-record, a list in predicted next season team-record format.
;;;   leaving-player, a list in player-record format.
;;;   coming-player, a list in player-record format.
;;; Purpose:
;;;   for each entry of follwing categories:
;;;     "play time" "points"-"assists"-"rebound"-"blocks"-"steals"-"Turn Over"-"FGA"-"FGM"
;;;    we  subtract the stat of leaving player from the team stat and add the stat of coming player to team stat. After that we
;;;     multiply that to time-fraction(team's play time before/team's play time after) the trade. 
;;; Produce:
;;;   result, updated team-record.
;;; Pre-conditions:
;;;   for the result to be meaningful, player-record should be diluted.
;;;    team-record should be predicted for next season (using code in predicting sector)
;;; Post-condition:
;;;   The team name in result is same as team name in input team-record.
(define update-team-stat
  (lambda (team-record leaving-player coming-player)
    (let ([team-stat (drop team-record 1)]
          [header (take team-record 1)]
          [leaving-player-stat (take (drop leaving-player 2) 9)]
          [coming-player-stat (take (drop coming-player 2) 9)]
          [time-fraction (/ (cadr team-record)                      ;calculate time fraction
                            (- (+ (cadr team-record)
                                  (caddr coming-player))
                               (caddr leaving-player)))])
      (append header
              (list (- (+ (cadr team-record)      ;team's playtime before trade
                          (caddr coming-player))  ;add coming player's play time
                       (caddr leaving-player)))   ;subtract to player   -> update team's time
              (map (o round (section * time-fraction <> ))       
                         (map -
                              (map +
                                   (drop team-stat 1)            ;remove play time from stat, we dealed with that seperately
                                   (drop coming-player-stat 1))  ;remove play time from stat, we dealed with that seperately
                              (drop leaving-player-stat 1)))     ;remove play time from stat, we dealed with that seperately
                    ))))

;;; Procedure:
;;;   trade
;;; Parameters:
;;;   player1, a player-record.
;;;   player2, a player-record.
;;; Purpose:
;;;   update 2 teams' stats when we swap player1 and player2 using update-team-stat proc.
;;; Produce:
;;;   result, a list 4 team-records.
;;; Pre-conditions:
;;;   [no additional conds]
;;; Post-condition:
;;;   if player1 = player2, no change was made.
;;;   the order in resut is team1's stat before trade, team1's stat after trade, team2's stat before trade, team2's stat after trade.
(define trade
  (lambda (player1 player2)
    (let* ([player1-stat-diluted (assoc player1 database-all-players-statistics-all-season-diluted)]
           [player2-stat-diluted (assoc player2 database-all-players-statistics-all-season-diluted)]
           [team1 (cadr player1-stat-diluted)]
           [team2 (cadr player2-stat-diluted)]
           [team1-stat-next-season (assoc team1 database-all-teams-statistics-all-next-season)]
           [team2-stat-next-season (assoc team2 database-all-teams-statistics-all-next-season)])
      (list
       team1-stat-next-season                  ;team1's stat before trade
       (update-team-stat team1-stat-next-season
                         player1-stat-diluted
                         player2-stat-diluted) ;team1's stat after trade
                      
       team2-stat-next-season                  ;team2's stat before trade
       (update-team-stat team2-stat-next-season
                         player2-stat-diluted
                         player1-stat-diluted) ;team2's stat after trade
       ))))
;> (trade "Stephen Curry" "Blake Griffin")
;'(("GS" 19740 9276 2397 3558 610 655 1228 6957 3498)
;  ("GS" 20079 8868 2357 3673 611 590 1209 6913 3407)
;  ("DET" 20456 8127 2011 3696 323 619 1072 6697 3041)
;  ("DET" 20117 8524 2045 3577 317 685 1088 6734 3125))











;;;ASSESSMENT and RANKING
;order of weight: "total players' points"-"total players' assists"-"total players' rebound"-
;       "total players' blocks"-"total players' steals"-"total players' Turn Over"
(define ranking-weight (list 0.05 3 0.05 25 10 -6))

;;; Procedure:
;;;   calculate-rank-pts
;;; Parameters:
;;;   team-record, a list in format of team record.
;;; Purpose:
;;;   calculate rank point of team-record by: multiply each entry (of categories: "player's points"-"player's assists"-"player's rebound"-"player's blocks"-
;;;      "player's steals"-"player's Turn Over") with according number in ranking-weight. then sum them up.
;;; Produce:
;;;   result, a number.
;;; Pre-condition:
;;;   team-record must not be empty
;;; Post-condition:
;;;   [no additional]
(define calculate-rank-pts
  (lambda (team-record)
    (let ([header (take team-record 1)]
          [mid (take (drop team-record 2) 6)]) ;take the entries that matter
      (append
       header
       (list (reduce + (map * mid ranking-weight)))))))

;;; Procedure:
;;;   compare-team-rank>?
;;; Parameters:
;;;   team-rank1, a pair of formate (team's name . team's rank point)
;;;   team-rank2, a pair of formate (team's name . team's rank point)
;;; Purpose:
;;;   compare the rank points of 2 teams.
;;; Produce:
;;;   result, a boolean value if rank of team 1 is > than rank of team 2.
;;; Pre-conditions:
;;;   [no additional]
;;; Post-conditions:
;;    [no additional]
(define compare-team-rank>?
  (lambda (team-rank1 team-rank2)
    (> (cadr team-rank1) (cadr team-rank2))))

;;; Procedure:
;;;  rank-all-teams
;;; Parameters:
;;;  database, a list contains team-records.
;;;    to evaluate the trade)
;;; Purpose:
;;;   apply calculate-rank-pts on each record of the database and sort them.
;;; Produce:
;;;   result, a list of pairs in format (team's name . team's rank point)
;;; Pre-conditions:
;;;   [no additional]
;;; Post-conditions:
;;;   the length of result is = the length of database.
;;;   for each team in database we have 1 pair in result.
;;;   th result is sorted by rank point, from high to low.
(define rank-all-teams
  (lambda (database)
    (sort
     (map calculate-rank-pts database)
     compare-team-rank>?)))

;;; predefined database of ranked teams based on their 2017-2018 season's stats.
(define ranked-2017-2018-season
  (rank-all-teams database-all-teams-statistics-all-season))
;> (take ranked-2017-2018-season 5)
;'(("GS" 22333.65) ("TOR" 19232.7) ("NO" 19070.05) ("MIA" 18113.85) ("MIL" 17985.25))

;;; predefined database of ranked teams based on predicted next season's stats.
(define ranked-2018-2019-season
  (rank-all-teams database-all-teams-statistics-all-next-season))
;> (take ranked-2018-2019-season 5)
;'(("GS" 22264.7) ("NO" 19149.8) ("TOR" 18809.5) ("MIL" 17792.9) ("SA" 17672.7))






;;; Procedure:
;;;   calculate-rank-trade.
;;; Parameters:
;;;   team-name-1, a string, name of the team that player-1 is on
;;;   player1, a string, name of player1
;;;   team-name-2, a string, name of the team that player-2 is on
;;;   player2, a string, name of player2
;;; Purpose:
;;;   produces a list of the new rankings after the trade.
;;; Produce:
;;;   result, list, a new list.
;;; Pre-conditions:
;;;   [no additional]
;;; Post-conditions:
;;;    the projected points for team-1 after the trade will be in the list in the format (team-name-1 projected-points)
;;;    the projected points for team-2 after the trade will be in the list in the format (team-name-2 projected-points)
;;;    the list will be sorted from the greatest projected points to the smallest projected points


(define calculate-rank-trade
  (lambda (team-name-1 player1 team-name-2 player2 )
    (sort (append (list (cadr (map calculate-rank-pts (trade player1 player2))))
                  (list (cadddr (map calculate-rank-pts (trade player1 player2))))
            (get-records-team team-name-1 team-name-2 (rank-all-teams database-all-teams-statistics-all-season))) new-sort)))

;;; Procedure:
;;;   city-city<?
;;; Parameters:
;;;   team1, a list
;;;   team2, a list
;;; Purpose:
;;;   Compare two teams based off their score.
;;; Produces:
;;;   1st?, a Boolean value
;;; Preconditions:
;;;   team1 and team2 are lists of the form 
;;;     (team1:string, score) (team-2 score)
;;; Postconditions:
;;;   * If the points scored by team1 is greater than team two, 1st? is #t.
;;;   * Otherwise, 1st? is #f.

;;; The documentation for the above code is adapted from the following link: https://www.cs.grinnell.edu/~curtsinger/teaching/2018F/CSC151/readings/tables.html
(define new-sort
  (lambda (team1 team2)
    (>(cadr team1) (cadr team2))))


;;; Procedure:
;;;  team-name?
;;; Parameters:
;;;  team-name, a string - name of team.
;;;  database, a list in format of '((team-name score)...).
;;; Purpose:
;;;  check if the team-name appears in the front of the database
;;; Produce:
;;;  result, a boolean value, either true or false.
;;; Pre conditions:
;;;  [no additional cond]
;;; Pros-conditions:
;;;  [no additional cond]
(define team-name?
  (lambda (team-name database)
    (equal? (car database) team-name)))

;;; Produce:
;;;  get-records-team
;;; Parameters:
;;;  name-1, a string- a team name.
;;;  name-2, a string- a team name.
;;;  database, a list containing the team name and the score in this format '((name-1 score)...)
;;; Purpose:
;;;  return a list of values that contain all but the the names and scores associated with name-1 and name-2.
;;; Produce:
;;;  result, a list .
;;; Pre-conditionals:
;;;  [no additional conds]
;;; Post-conditionals:
;;;  if name-1 or name-2 does not exist in database, the result is null.
;;;  the length of result = the number of records in database belong to team minus two.

(define get-records-team
  (lambda (name1 name2 database)
    (filter (negate (section team-name? name2  <>)) (filter (negate (section team-name? name1 <>)) database))))

;;; Procedure:
;;;   trade-result
;;; Parameters:
;;;   team-name-1, a string, name of the team that player-1 is on
;;;   player1, a string, name of player1
;;;   team-name-2, a string, name of the team that player-2 is on
;;;   player2, a string, name of player2
;;; Purpose:
;;;   Determine whether or not a trade is good for the team-name-1
;;; Produce:
;;;   result, a string.
;;; Pre-conditions:
;;;   [no additional conds]
;;; Post-conditions:
;;;   If the rank of team-name-1 increases after the trade, then result = "the trade is good".
;;;   If the rank of team-name-1 decreases after the trade, then result = "the trade is bade".
;;;   If the rank of team-name-1 stays the same after the trade, then result = "the change is not significant".
(define trade-result
  (lambda (team-name-1 player1 team-name-2 player2 )
    (cond
      [(> (index-of team-name-1 (map car (calculate-rank-trade team-name-1 player1 team-name-2 player2)) )
          (index-of team-name-1 (map car (rank-all-teams database-all-teams-statistics-all-season)) ))
       "the trade is good"]
      [(< (index-of team-name-1 (map car (calculate-rank-trade team-name-1 player1 team-name-2 player2)) )
          (index-of team-name-1 (map car (rank-all-teams database-all-teams-statistics-all-season)) ))
       "the trade is bad"]
      [else "The change is not significant"])))
       