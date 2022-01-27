
#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title 
#'		Select a door
#' @description
#' 	`select_door()` selects one door of the three options
#'
#' @details
#' 	only one door can be selected, this will be the first door that is picked, 
#'  the contestant will have a chance to change doors if desired later in the game.
#'
#' @param ... no arguments are used by the function.
#' 
#' @return the function returns a number between 1 and 3 to represent one of the three doors
#'
#' @examples
#' 	select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title 
#' 		Open a Goat Door
#'
#' @description
#'		Time to open one door that contains a "goat", 
#'  	this door cannot be the door that was selected
#' 		by the contestant as their first pick. 
#'
#' @details
#' 		If the contestant originally selected the car as their pick, 
#' 		then either of the two remaining doors can be opened as 
#' 		each would contain a goat. If the contestant chose a goat,
#' 		then only the other remaining goat door can be opened. 
#'
#' @param ... no arguments are used by the function.
#'
#' @return  the function returns a number between 1 and 3 to represent one of the three doors 
#'
#' @examples
#'		open_goat_door()
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title 
#'		Want to stay or switch doors?
#'
#' @description
#'		The contestant will select their final pick
#'
#' @details
#'		The contestant can either choose to stick with 
#'		their original pick or switch to the other remaining
#'		closed door. 
#'
#' @param ... no arguments are used by the function.
#'
#' @return  the function returns a number between 1 and 3 to represent one of the three doors 
#'
#' @examples
#'		change_door()
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title 
#' 		Winner?
#'
#' @description
#'		Determine is contestant chose the door with the car.
#'
#' @details
#' 		If the contestant has the door with the car then they win,
#' 		if they have a door with a goal then they lose. 
#'
#' @param ... no arguments are used by the function.
#'
#' @return the function returns a string of either "WIN" or "LOSE"
#'
#' @examples
#'		determine_winner()
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title 
#'		Let's Play!
#'
#' @description
#'		Run through the whole game from start to finish.
#'
#' @details
#'		Create a new game, select a door, open a goat door, 
#'		choose whether the contestant wants to stay or switch,
#'		determine if the contestant wins. 
#'
#' @param ... no arguments are used by the function.
#'
#' @return the function returns a dataframe of "WIN" and "LOSE" and "Switch" and "Stay"
#'
#' @examples
#'		play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title 
#'		Loops
#'
#' @description
#'		Complete n number of loops to simulate playing the game n times.
#'
#' @details
#'		Create loops of playing the game to determine the outcome over
#'		many iterations. Can determine the if its more optimal to stick with 
#'		orginal door or swtich doors. 
#'
#' @param ... no arguments are used by the function.
#'
#' @return the function returns a dataframe of "WIN" and "LOSE" and "Switch" and "Stay"
#'
#' @examples
#'		play_n_games()
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
