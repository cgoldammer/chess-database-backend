-- General overview over the games and evaluations in the database
SELECT 
  db.name as database
, count(distinct g.id) as games
, count(distinct me.game_id) as games_evaluated
, sum((me.id is not null)::Int) as number_evals
FROM game g 
JOIN database db ON db.id=g.database_id
LEFT JOIN move_eval me ON g.id=me.game_id
GROUP BY db.name;
