library(tidygraph)
library(tidyverse)
library(ggraph)

legal_moves <- function(gamestate) {
  turn <- gamestate[1]
  #sets the indices to pull the active player's state
  if (turn) {
    active_state <- gamestate[4:5]
    active_start <- 4
    inactive_state <- gamestate[2:3]
    inactive_start <- 2
  } else {
    active_state <- gamestate[2:3]
    active_start <- 2
    inactive_state <- gamestate[4:5]
    inactive_start <- 4
  }
  
  #if a gamestate is a winning state, that should be the only valid option for a move
  #a gamestate is a winning state if a single hit ends the game (the nonactive player has one hand)
  if (inactive_state[2] == 0 & 
      ((active_state[1] + inactive_state[1]) == 5 || 
       (active_state[2] + inactive_state[1]) == 5)) {
    output <- list(c(turn, 0, 0, 0, 0))
    output_df <- data.frame(states = I(list(gamestate)),
                            end = I(output))
    return(output_df)
  }
  
  output <- list()
  
  #compliling all combinations of hits (with duplicates)
  hit_list <- list()
  #i runs through the inactive player's hands, j through the active player's
  for (i in 1:2) {
    #a hand with no fingers up can't be hit
    if (inactive_state[i] != 0) {
      for (j in 1:2) {
        temp_state_hit <- inactive_state
        temp_state_hit[i] <- (active_state[j] + inactive_state[i]) %% 5
        temp_state_hit <- list(sort(temp_state_hit, decreasing = TRUE))
        hit_list <- append(hit_list, temp_state_hit)
      }
    }
  }
  #removing duplicates and unchanged states from hit list
  hit_list <- hit_list[!sapply(hit_list, identical, inactive_state)]
  hit_list <- unique(hit_list)
  for (i in 1:length(hit_list)) {
    temp_state <- gamestate
    #replacing the inactive player's state with the post-hit state
    temp_state[inactive_start:(inactive_start+1)] <- hit_list[[i]]
    #the output states are all during the start of the inactive player's turn
    temp_state[1] <- 1 - temp_state[1]
    move <- list(temp_state)
    output <- append(output, move)
  }
  
  #all ways to get from the key state to other states via rearrangements
  #note: R has no dictionary object, so a keyed list is the next best thing
  rearrangements <- list(
    "1&0" = NULL, "2&0" = c(1,1), "3&0" = c(2,1), "4&0" = list(c(2,2),c(3,1)),
    "1&1" = c(2,0), "2&1" = c(3,0), "3&1" = list(c(2,2),c(4,0)), "4&1" = c(3,2),
    "2&2" = list(c(3,1),c(4,0)), "3&2" = c(4,1), "4&2" = c(3,3),
    "3&3" = c(4,2), "4&3" = NULL,
    "4&4" = NULL
  )
  
  #putting active state into the form of the list keys
  string_state <- paste(active_state, collapse = "&")
  #this condition and only this condition implies exactly two rearrangements
  if (sum(active_state) == 4) {
    for (k in 1:2) {
      temp_state_arrange <- gamestate
      #replacing the active state with a rearrangement
      temp_state_arrange[active_start:(active_start+1)] <- rearrangements[string_state][[1]][[k]]
      temp_state_arrange[1] <- 1 - temp_state_arrange[1]
      move <- list(temp_state_arrange)
      output <- append(output, move)
    }
    #no possible rearrangements outside of these conditions
  } else if (sum(active_state)>1 & sum(active_state)<7){
    temp_state_arrange <- gamestate
    temp_state_arrange[active_start:(active_start+1)] <- rearrangements[string_state][[1]]
    temp_state_arrange[1] <- 1 - temp_state_arrange[1]
    move <- list(temp_state_arrange)
    output <- append(output, move)
  }
  #priming the output to be converted to a graph
  output_df <- data.frame(states = I(list(gamestate)[rep(1,length(output))]),
                          end = I(output))
  output_df
}

all_states <- list()
#each possible single player state, 14 total
single_state <- list(c(1,0),c(2,0),c(3,0),c(4,0),
                     c(1,1),c(2,1),c(3,1),c(4,1),
                     c(2,2),c(3,2),c(4,2),
                     c(3,3),c(4,3),
                     c(4,4))
total_single_states <- length(single_state)
#each player's state is (more or less) independent of the other's, so 14^2 total (ignoring turn)
for (i in 1:total_single_states) {
  for (j in 1:total_single_states) {
    #392 states total, factoring in turn
    position0 <- list(c(0,single_state[[i]],single_state[[j]]))
    position1 <- list(c(1,single_state[[i]],single_state[[j]]))
    all_states <- c(all_states, position0, position1)
  }
}
total_states <- length(all_states)
#adding each player's win state
all_states <- append(all_states, list(c(0,0,0,0,0), c(1,0,0,0,0)))
#indexing each state for ease
nodes <- data.frame(id = 1:(total_states + 2), states = I(all_states))

inter_df <- data.frame(states = numeric(), end = list())
#finding the connections between each gamestate
for (k in 1:total_states) {
  inter_df <- rbind(inter_df, legal_moves(all_states[[k]]))
}

#formatting edges for graphical use (integer id values rather than the vectors themselves)
edges <- nodes %>% inner_join(inter_df, by = "states")
colnames(edges) <- c("id", "start", "states")
edges <- edges %>% inner_join(nodes, by = "states") %>% select(id.x,id.y)
colnames(edges) <- c("in_id", "out_id")

#conversion to graph object
chopsticks_graph <- tbl_graph(nodes = nodes, edges = edges)

#coloring will be based on the number of possible moves from a gamestate
chopsticks_colored <- chopsticks_graph %>% activate(nodes) %>% mutate(deg = centrality_degree())

#creating and saving graph of gamestate relations
#note: brown3 shows up very well on most colors without hurting the eyes!
png("large_graph.png", width = 2000, height = 2000, res = 150)
chopsticks_colored %>% 
  ggraph(layout = "stress") +
  geom_edge_link(color = "gray80", width = 0.5) +
  geom_node_point(aes(color = deg), size = 5.5) +
  geom_node_text(aes(label = id), size = 2.5, color = "brown3") +
  scale_color_viridis_c(name = "Edge Count") +
  scale_size(guide = "none") +
  theme_graph()
dev.off()

forced_states_0 <- unique((edges %>% filter(out_id == 393))$in_id)
kf_states_0 <- c()
changed <- TRUE
i <- 1
while (!(121 %in% forced_states_0 || 122 %in% forced_states_0) & changed) {
  start_length <- length(forced_states_0)
  trimmed_edges <- edges %>% filter(!(in_id %in% forced_states_0) & out_id != 394)
  
  if (i %% 2) {
    select_edges <- trimmed_edges %>%
      filter(in_id %% 2 == 0) %>%
      group_by(in_id) %>% 
      filter(sum(!(out_id %in% forced_states_0)) == 1) %>%
      ungroup()
    new_kf_states <- unique(select_edges$in_id)
    kf_states_0 <- unique(c(kf_states_0, new_kf_states))
    
    trimmed_edges <- trimmed_edges %>%
      filter(in_id %% 2 == 0) %>%
      group_by(in_id) %>% 
      filter(all(out_id %in% forced_states_0)) %>%
      ungroup()
    new_forced_states <- unique(trimmed_edges$in_id)
    forced_states_0 <- c(forced_states_0, new_forced_states)
  } else {
    trimmed_edges <- trimmed_edges %>% 
      filter(in_id %% 2 == 1) %>%
      group_by(in_id) %>% 
      filter(any(out_id %in% forced_states_0)) %>%
      ungroup()
    new_forced_states <- unique(trimmed_edges$in_id)
    forced_states_0 <- c(forced_states_0, new_forced_states)
  }
  
  end_length <- length(forced_states_0)
  if (end_length == start_length) {
    changed <- FALSE
  }
  i <- i + 1
}

forced_states_1 <- unique((edges %>% filter(out_id == 394))$in_id)
kf_states_1 <- c()
changed <- TRUE
j <- 1
while (!(121 %in% forced_states_1 || 122 %in% forced_states_1) & changed) {
  start_length <- length(forced_states_1)
  trimmed_edges <- edges %>% filter(!(in_id %in% forced_states_1) & out_id != 393)
  
  if (j %% 2) {
    select_edges <- trimmed_edges %>%
      filter(in_id %% 2 == 1) %>%
      group_by(in_id) %>% 
      filter(sum(!(out_id %in% forced_states_1)) == 1) %>%
      ungroup()
    new_kf_states <- unique(select_edges$in_id)
    kf_states_1 <- unique(c(kf_states_1, new_kf_states))
    
    trimmed_edges <- trimmed_edges %>%
      filter(in_id %% 2 == 1) %>%
      group_by(in_id) %>% 
      filter(all(out_id %in% forced_states_1)) %>%
      ungroup()
    new_forced_states <- unique(trimmed_edges$in_id)
    forced_states_1 <- c(forced_states_1, new_forced_states)
  } else {
    trimmed_edges <- trimmed_edges %>% 
      filter(in_id %% 2 == 0) %>%
      group_by(in_id) %>% 
      filter(any(out_id %in% forced_states_1)) %>%
      ungroup()
    new_forced_states <- unique(trimmed_edges$in_id)
    forced_states_1 <- c(forced_states_1, new_forced_states)
  }
  
  end_length <- length(forced_states_1)
  if (end_length == start_length) {
    changed <- FALSE
  }
  j <- j + 1
}

chopsticks_endstates <- chopsticks_graph %>% 
  activate(nodes) %>% 
  mutate(endstate = as.factor(ifelse(
    id %in% forced_states_0, 1, ifelse(
      id %in% forced_states_1, -1, 0))))
png("large_graph_endstates.png", width = 2000, height = 2000, res = 150)
chopsticks_endstates %>% 
  ggraph(layout = "stress") +
  geom_edge_link(color = "gray80", width = 0.5) +
  geom_node_point(aes(color = endstate), size = 5.5) +
  geom_node_text(aes(label = id), size = 2.5, color = "brown3") +
  scale_color_ordinal(name = "Who Wins") +
  scale_size(guide = "none") +
  theme_graph()
dev.off()