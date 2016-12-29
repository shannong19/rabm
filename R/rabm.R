#' Main function for running ABM
#'
#' @param agents a data frame where each row corresponds to one agent with characteristics.  The first column should be a unique ID, called "id".  Additionally, it is required that the agents have a longitude and latitude location called "lon" and "lat", respectively.  Other columns should include personal attributes such as race, age, income, etc.  These variables should all begin with the prefix "p_".  The data frame should also have environmental assignments such as households, schools, workplaces, hospitals, etc.  These variables should all begin with the prefix "e_". 
#' @return
#' @details This function takes in A B C and runs the ABM for a designated number of time steps.  The return is X Y Z.
rabm <- function(agents, environment,
                 disease_params,
                 update_fxn, summarize_fxn,
                 total_time, time_step = 1, return_vars){
    ## create time sequence, beginning at 0 + time_step
    time_seq <- seq(time_step, total_time, by = time_step)
    ## initialize summary list
    summary_list <- vector(mode = "list", length = (time_seq + 1))
    ## fill in with initial known parameters
    summary_list[1] <- summarize_fxn(agents)
    ## loop over the time steps
    for (tt in 1:length(time_seq)){
        agents <- update_fxn(agents, environment)
        summary_list[1 + tt] <- summarize_fxn(agents)
    }
    return(summary_list)
}


#' A more realistic epidemic update function
#'
#' @param
#' @return agents with updated infection status (along with other variables)
update_epidemic <- function(agents, environment,
                            update_agent_internals,
                            update_agent_externals, internal_warnings,
                            environment_closings){
    ## First change agents inherent characteristics, e.g.,  how likely are they to go to each environment, whether they were innoculated, etc., how long the incubation period is, whether they are infectious, how long they have had the disease, etc.
    agents <- update_agent_internals(agents, environment, internal_warnings)
    ## Next have the agents interact with one another.  They go to their environments with certain probability, infect one another with certain probability, etc.
    agents <- update_agent_externals(agents, environment, environment_closings)
    return(agents)
}
                            
                             

