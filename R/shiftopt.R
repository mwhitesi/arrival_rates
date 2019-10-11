# Name: shift_targets.R
# Desc: Pick shifts to meet demand
# Author: Matt Whiteside
# Date: Dec 11, 2018

shiftopt__optimize <- function(required.servers, shift.setup, solver="glpk") {
  
  # Subtract demand that can be addressed by weekly repeating consistent shifts first
  
  # Schedule the 24/7 demand first
  n247 = min(required.servers$s)
  required.servers %<>% mutate(s = s - n247)
  
  
  # Available shifts
  sfts = shifts$shift_options(shift.setup$shift.types, shift.setup$period.stagger, shift.setup$period.days,
                              shift.setup$daily.sc.window)
  a = sfts$shifts
  cost = sfts$cost
  s = sfts$changeover
  
  # May be overkill, if j is only a single index
  a_fun <- function(i, j) {
    combos <- CJ(i,j)
    ompr::colwise(combos[, a[V1,V2], by = seq_len(nrow(combos))]$V1)
  }
  
  s_fun <- function(i, j) {
    combos <- CJ(i,j)
    ompr::colwise(combos[, s[V1,V2], by = seq_len(nrow(combos))]$V1)
  }
  
  # Requirements
  # Convert from weekday/weekend format to weekly matrix
  if(shift.setup$period.days == 7) {
    r = shiftopt__required_matrix2(required.servers, shift.setup$period.stagger, shift.setup$period.days)
  } else {
    stop(sprintf('Periods of length %i currently not available', shift.setup$period.days))
  }
  
  # Solve for shifts
  ns=dim(a)[1]
  np=dim(a)[2]
  maxsc = shift.setup[['max.sc']]
  
  optm <- MILPModel() %>%
    add_variable(x[i], i = 1:ns, type = "integer") %>%
    set_objective(sum_expr(ompr::colwise(cost[i])*x[i], i = 1:ns), "min") %>%
    set_bounds(x[i], i=1:ns, lb = 0)
  
  # Required units constraint
  for(j in 1:np) {
    optm = add_constraint(optm, sum_expr(a_fun(i, j)*x[i], i=1:ns) >= r[j])
    optm = add_constraint(optm, sum_expr(s_fun(i, j)*x[i], i=1:ns) <= maxsc)
  }
  
  #result = optm %>% solve_model(with_ROI(solver = solver))
  
  # Set a time limit
  result = optm %>% solve_model(with_ROI(solver = solver, verbosity=1, time_limit=5*60))
  
  soln = data.table(get_solution(result, x[i]))
  soln = soln[value != 0]
  
  shift.summary = soln[,.(
    type=rep(sfts$type[i], value),
    n=rep(value, value),
    cost=rep(sfts$costs[i], value)
  )]
  
  # Add back the 24/7 shifts
  shift.matrix = rbind(a[soln[,rep(i, value)],], matrix(1,nrow=n247,ncol=np))
  shift.summary = rbind(shift.summary, 
                        data.table(type=rep('week_247', n247),
                                   n=1, # Only one of each? Sure
                                   cost=rep(24*7, n247)
                        )
  )
  
  return(list(shifts=shift.summary, shift.matrix=shift.matrix))
  
}

shiftopt__optimize_with_upper_limit <- function(required.servers, shift.setup, solver="glpk") {
  # Optimize shifts so 
  
  
  # Subtract demand that can be addressed by weekly repeating consistent shifts first
  
  # Schedule the 24/7 demand first
  n247 = min(required.servers$s)
  required.servers %<>% mutate(s = s - n247)
  
  
  # Available shifts
  sfts = shifts$shift_options(shift.setup$shift.types, shift.setup$period.stagger, shift.setup$period.days,
                              shift.setup$daily.sc.window)
  a = sfts$shifts
  cost = sfts$cost
  s = sfts$changeover
  
  # May be overkill, if j is only a single index
  a_fun <- function(i, j) {
    combos <- CJ(i,j)
    print(names(combos))
    ompr::colwise(combos[, a[i,j], by = seq_len(nrow(combos))]$V1)
  }
  
  a_fun2 <- function(i, j) {
    print(length(i))
    print(length(j))
    combos <- CJ(i,j)
    print(combos)
    ompr::colwise(combos[, a[i,j], by = seq_len(nrow(combos))]$V1)
  }
  
  s_fun <- function(i, j) {
    combos <- CJ(i,j)
    ompr::colwise(combos[, s[V1,V2], by = seq_len(nrow(combos))]$V1)
  }
  
  
  
  # Requirements
  if(shift.setup$period.days == 7) {
    r = shiftopt__required_matrix2(required.servers, shift.setup$period.stagger, shift.setup$period.days)
  } else {
    stop(sprintf('Periods of length %i currently not available', shift.setup$period.days))
  }
  
  # Solve for shifts
  ns=dim(a)[1]
  np=dim(a)[2]
  maxsc = shift.setup[['max.sc']]
  
  optm <- MILPModel() %>%
    add_variable(x[i], i = 1:ns, type = "integer", lb=0) %>%
    #add_variable(delta[j], j = 1:np, type = "continuous", lb=0) %>%
    #add_variable(delta, type = "continuous", lb=0) %>%
    add_variable(delta, type = "continuous") %>%
    #set_objective(sum_expr(delta[j], j = 1:np), "min") %>%
    set_objective(delta, "min") %>%
    add_constraint(sum_expr(ompr::colwise(cost[i])*x[i], i = 1:ns) <= 3000)
  
  for(j in 1:np) {
    # optm = add_constraint(optm, r[j] - sum_expr(ompr::colwise(a[i, j]) * x[i], i=1:ns) <= delta[j])
    # optm = add_constraint(optm, r[j] - sum_expr(ompr::colwise(a[i, j]) * x[i], i=1:ns) >= -delta[j])
    optm = add_constraint(optm, r[j] - sum_expr(ompr::colwise(a[i, j]) * x[i], i=1:ns) <= delta)
    #optm = add_constraint(optm, r[j] - sum_expr(ompr::colwise(a[i, j]) * x[i], i=1:ns) >= -delta)
    #optm = add_constraint(optm, sum_expr(s_fun(i, j)*x[i], i=1:ns) <= maxsc)
  }
  
  #result = optm %>% solve_model(with_ROI(solver = solver))
  
  # Set a time limit
  result = optm %>% solve_model(with_ROI(solver = solver, verbosity=1, time_limit=5*60))
  
  soln = data.table(get_solution(result, x[i]))
  soln = soln[value != 0]
  
  shift.summary = soln[,.(
    type=rep(sfts$type[i], value),
    n=rep(value, value),
    cost=rep(sfts$costs[i], value)
  )]
  
  # Add back the 24/7 shifts
  shift.matrix = rbind(a[soln[,rep(i, value)],], matrix(1,nrow=n247,ncol=np))
  shift.summary = rbind(shift.summary, 
                        data.table(type=rep('week_247', n247),
                                   n=1, # Only one of each? Sure
                                   cost=rep(24*7, n247)
                        )
  )
  
  return(list(shifts=shift.summary, shift.matrix=shift.matrix))
  
}



shiftopt__required_matrix2 <- function(required.servers, period.stagger, period.days) {
  # Map the format output from queuemodel__model to the required matrix
  # Model outputs could change and this function will need to be updated
  # Input Format: Tibble with columns group=(1:7), ts=, s=# of servers
  
  # Specifically designed for 1 week scheduling
  stopifnot(period.days == 7)
  
  nperiods = 24*60*period.days/period.stagger
  
  # Map out each required server to the periods used in the optimization
  r = array(0, dim=nperiods)
  
  day.shift = 24*60/period.stagger
  required.servers %<>% group_by(group, tp) %>% mutate(i=day.shift*(group-1)+tp)
  
  r[required.servers$i] = required.servers$s
  
  return(r)
}

shiftopt__required_matrix1 <- function(required.servers, period.stagger, period.days) {
  # Map the format output from queuemodel_model to the required matrix
  # Model outputs could change and this function will need to be updated
  # Input Format: Tibble with columns group=("weekday"|"weekend"), hour=0:23, s=# of servers
  
  # Specifically designed for 1 week scheduling
  stopifnot(period.days == 7)
  
  nperiods = 24*60*period.days/period.stagger
  
  # Starting on Sunday, map out each required server to the periods used in the optimization
  r = array(0, dim=nperiods)
  
  # Sunday & Saturday
  day.shift = 24*60/period.stagger
  sat.shift = 24*60*6/period.stagger
  for(p in 1:day.shift) {
    m=(p-1)*period.stagger
    h = m %/% 60
    p2 = sat.shift + p
    
    #print(sprintf("%i, %i, %i, %d",p,p2,m,h))
    s = required.servers %>% filter(hour == h & group == "weekend") %>% pull(s)
    
    r[c(p,p2)] = s
    
  }
  
  # Monday-Friday
  for(p in 1:day.shift) {
    m=(p-1)*period.stagger
    h = m %/% 60
    
    s = required.servers %>% filter(hour == h & group == "weekday") %>% pull(s)
    i = p+1:5*day.shift
    r[i] = s
    
    #print(sprintf("%i, %s, %i, %d",p,paste(i,collapse=','),m,h))
    
  }
  
  # Quick check
  #ggplot(data.frame(i=1:nperiods,r=r), aes(i, r)) + geom_line()
  
  return(r)
  
}

optimumExperiment <- function(required.servers) {
  
  dir.create('data/interim/optimum_experiment', showWarnings=FALSE)
  
  demand.table = required.servers
  period.stagger = 15
  period.days = 7
  
  solver <- 'symphony'
  
  # Schedule the 24/7 demand separately for all scenarios
  n247 = min(required.servers$s)
  required.servers %<>% mutate(s = s - n247)
  np = period.days * 24 * 60 / period.stagger
  
  n247.matrix = matrix(1,nrow=n247, ncol=np)
  n247.summary = data.table(type=rep('week_247', n247),
                            n=n247,
                            cost=rep(24*7, n247))
  
  # Scenario 1 - Funded EDMO 911 shifts
  shift.setup1 <- list(
    shift.types = c('week_12hr', '4day_10.5hr', '2day_10.5hr'),
    daily.sc.window = c(5*60,20*60),
    max.sc = 4,
    period.stagger = period.stagger,
    period.days = period.days
  )
  r1 = scenario1(required.servers, shift.setup1, solver)
  r1$shift.matrix = rbind(r1$shift.matrix, n247.matrix)
  
  p11 = shifts$plot_weekly_shift_gantt(r1$shift.summary, r1$shift.matrix, period.stagger)
  w=dim(r1$shift.summary)[1]/8
  ggsave('data/interim/optimum_experiment/s1_gantt.pdf', p11, device='pdf', width=12, height=w)
  p12 =shifts$plot_shifts_vs_demand(r1$shift.summary, r1$shift.matrix, period.stagger, demand.table)
  ggsave('data/interim/optimum_experiment/s1_scheduled_vs_required_units.pdf', p12, device='pdf', width=11, height=8.5)
  r11 = shifts$evaluationSummary(r1$shift.matrix, period.stagger, demand.table)
  cat('\n\n-------------\nScenario 1')
  invisible(lapply(seq_along(r11), function(i) { cat(sep="", "\n", names(r11)[i], ": ") ; print(r11[[i]]) }))
  print(r1$shift.summary %>% count(type))
  
  # Scenario 2 - Open
  shift.setup2 <- list(
    shift.types = c('12hr', '10.5hr', '8.4hr'),
    daily.sc.window = c(5*60,20*60),
    max.sc = 4,
    period.stagger = period.stagger,
    period.days = period.days
  )
  r2 = scenario1(required.servers, shift.setup2, solver)
  r2$shift.summary = rbind(r2$shift.summary, n247.summary)
  r2$shift.matrix = rbind(r2$shift.matrix, n247.matrix)

  p21 = shifts$plot_weekly_shift_gantt(r2$shift.summary, r2$shift.matrix, period.stagger)
  w=dim(r2$shift.summary)[1]/8
  ggsave('data/interim/optimum_experiment/s2_gantt.pdf', p21, device='pdf', width=12, height=w)
  p22 =shifts$plot_shifts_vs_demand(r2$shift.summary, r2$shift.matrix, period.stagger, demand.table)
  ggsave('data/interim/optimum_experiment/s2_scheduled_vs_required_units.pdf', p22, device='pdf', width=11, height=8.5)
  r21 = shifts$evaluationSummary(r2$shift.matrix, period.stagger, demand.table)
  cat('\n\n-------------\nScenario 2')
  invisible(lapply(seq_along(r21), function(i) { cat(sep="", "\n", names(r21)[i], ": ") ; print(r21[[i]]) }))
  print(r2$shift.summary %>% count(type))
  
  # Scenario 3 - Greedy
  shift.setup3 <- list(
    shift.types = c('week_12hr', '10.5hr'),
    daily.sc.window = c(5*60,20*60),
    max.sc = 4,
    period.stagger = period.stagger,
    period.days = period.days
  )
  r3 = scenario2(required.servers, shift.setup3, solver)
  r3$shift.summary = rbind(r3$shift.summary, n247.summary)
  r3$shift.matrix = rbind(r3$shift.matrix, n247.matrix)
  
  p31 = shifts$plot_weekly_shift_gantt(r3$shift.summary, r3$shift.matrix, period.stagger)
  w=dim(r3$shift.summary)[1]/8
  ggsave('data/interim/optimum_experiment/s3_gantt.pdf', p31, device='pdf', width=12, height=w)
  p32 =shifts$plot_shifts_vs_demand(r3$shift.summary, r3$shift.matrix, period.stagger, demand.table)
  ggsave('data/interim/optimum_experiment/s3_scheduled_vs_required_units.pdf', p32, device='pdf', width=11, height=8.5)
  r31 = shifts$evaluationSummary(r3$shift.matrix, period.stagger, demand.table)
  cat('\n\n-------------\nScenario 3')
  invisible(lapply(seq_along(r31), function(i) { cat(sep="", "\n", names(r31)[i], ": ") ; print(r31[[i]]) }))
  print(r3$shift.summary %>% count(type))
  
  
  # Scenario 4 - Open - increased maxsc
  shift.setup4 <- list(
    shift.types = c('12hr', '10.5hr', '8.4hr'),
    daily.sc.window = c(5*60,20*60),
    max.sc = 100,
    period.stagger = period.stagger,
    period.days = period.days
  )
  r4 = scenario1(required.servers, shift.setup4, solver)
  r4$shift.summary = rbind(r4$shift.summary, n247.summary)
  r4$shift.matrix = rbind(r4$shift.matrix, n247.matrix)
  
  p41 = shifts$plot_weekly_shift_gantt(r4$shift.summary, r4$shift.matrix, period.stagger)
  w=dim(r4$shift.summary)[1]/8
  ggsave('data/interim/optimum_experiment/s4_gantt.pdf', p41, device='pdf', width=12, height=w)
  p42 =shifts$plot_shifts_vs_demand(r4$shift.summary, r4$shift.matrix, period.stagger, demand.table)
  ggsave('data/interim/optimum_experiment/s4_scheduled_vs_required_units.pdf', p42, device='pdf', width=11, height=8.5)
  r41 = shifts$evaluationSummary(r4$shift.matrix, period.stagger, demand.table)
  cat('\n\n-------------\nScenario 4')
  invisible(lapply(seq_along(r41), function(i) { cat(sep="", "\n", names(r41)[i], ": ") ; print(r41[[i]]) }))
  print(r4$shift.summary %>% count(type))
  
  # Scenario 5 - EDMO 911 Funded - increased maxsc
  shift.setup5 <- list(
    shift.types = c('week_12hr', '4day_10.5hr', '2day_10.5hr'),
    daily.sc.window = c(5*60,20*60),
    max.sc = 100,
    period.stagger = period.stagger,
    period.days = period.days
  )
  r5 = scenario1(required.servers, shift.setup5, solver)
  r5$shift.summary = rbind(r5$shift.summary, n247.summary)
  r5$shift.matrix = rbind(r5$shift.matrix, n247.matrix)
  
  p51 = shifts$plot_weekly_shift_gantt(r5$shift.summary, r5$shift.matrix, period.stagger)
  w=dim(r5$shift.summary)[1]/8
  ggsave('data/interim/optimum_experiment/s5_gantt.pdf', p51, device='pdf', width=12, height=w)
  p52 =shifts$plot_shifts_vs_demand(r5$shift.summary, r5$shift.matrix, period.stagger, demand.table)
  ggsave('data/interim/optimum_experiment/s5_scheduled_vs_required_units.pdf', p52, device='pdf', width=11, height=8.5)
  r51 = shifts$evaluationSummary(r5$shift.matrix, period.stagger, demand.table)
  cat('\n\n-------------\nScenario 5')
  invisible(lapply(seq_along(r51), function(i) { cat(sep="", "\n", names(r51)[i], ": ") ; print(r51[[i]]) }))
  print(r5$shift.summary %>% count(type))
  
}

scenario1 <- function(required.servers, shift.setup, solver) {
  
  # Available shifts
  sfts = shifts$shift_options(shift.setup$shift.types, shift.setup$period.stagger, shift.setup$period.days,
                              shift.setup$daily.sc.window)
  a = sfts$shifts
  cost = sfts$cost
  s = sfts$changeover
  maxsc = shift.setup$max.sc
  
  # May be overkill, if j is only a single index
  a_fun <- function(i, j) {
    combos <- CJ(i,j)
    ompr::colwise(combos[, a[V1,V2], by = seq_len(nrow(combos))]$V1)
  }
  
  s_fun <- function(i, j) {
    combos <- CJ(i,j)
    ompr::colwise(combos[, s[V1,V2], by = seq_len(nrow(combos))]$V1)
  }
  
  # Requirements
  # Convert from weekday/weekend format to weekly matrix
  if(shift.setup$period.days == 7) {
    r = shiftopt__required_matrix2(required.servers, shift.setup$period.stagger, shift.setup$period.days)
  } else {
    stop(sprintf('Periods of length %i currently not available', shift.setup$period.days))
  }
  
  # Solve for shifts
  ns=dim(a)[1]
  np=dim(a)[2]
  
  optm <- MILPModel() %>%
    add_variable(x[i], i = 1:ns, type = "integer") %>%
    set_objective(sum_expr(ompr::colwise(cost[i])*x[i], i = 1:ns), "min") %>%
    set_bounds(x[i], i=1:ns, lb = 0)
  
  # Required units constraint
  for(j in 1:np) {
    optm = add_constraint(optm, sum_expr(a_fun(i, j)*x[i], i=1:ns) >= r[j])
    optm = add_constraint(optm, sum_expr(s_fun(i, j)*x[i], i=1:ns) <= maxsc)
  }
  
  #result = optm %>% solve_model(with_ROI(solver = solver))
  result = optm %>% solve_model(with_ROI(solver = solver, verbosity=1))
  
  soln = data.table(get_solution(result, x[i]))
  soln = soln[value != 0]
  
  shift.summary = soln[,.(
    type=rep(sfts$type[i], value),
    n=rep(value, value),
    cost=rep(sfts$costs[i], value)
  )]
  
  shift.matrix = a[soln[,rep(i, value)],]
  
  return(list(shift.summary=shift.summary, shift.matrix=shift.matrix))                      
  
}

greedy.fitting <- function(r, a, c, s, maxsc, solver) {
  
  a_fun <- function(i, j) {
    combos <- CJ(i,j)
    ompr::colwise(combos[, a[V1,V2], by = seq_len(nrow(combos))]$V1)
  }
  
  s_fun <- function(i, j) {
    combos <- CJ(i,j)
    ompr::colwise(combos[, s[V1,V2], by = seq_len(nrow(combos))]$V1)
  }
  
  # Solve for max shifts without going overcapacity
  ns=dim(a)[1]
  np=dim(a)[2]
  
  optm <- MILPModel() %>%
    add_variable(x[i], i = 1:ns, type = "integer") %>%
    set_objective(sum_expr(ompr::colwise(c[i])*x[i], i = 1:ns), "max") %>%
    set_bounds(x[i], i=1:ns, lb = 0)
  
  # Required units constraint
  for(j in 1:np) {
    optm = add_constraint(optm, sum_expr(a_fun(i, j)*x[i], i=1:ns) <= r[j])
    optm = add_constraint(optm, sum_expr(s_fun(i, j)*x[i], i=1:ns) <= maxsc[j])
  }
  
  result = optm %>% solve_model(with_ROI(solver = solver, verbosity=1))
  
  soln = data.table(get_solution(result, x[i]))
  soln = soln[value != 0]
  
  # update requirement and shift change counts
  shift.summary = soln[,.(
    type=rep(sfts$type[i], value),
    n=rep(value, value),
    cost=rep(sfts$costs[i], value)
  )]
  
  idx = soln[,rep(i, value)]
  shift.matrix = a[idx,]
  new.maxsc = maxsc - apply(s[idx,],2,sum)
  new.r = r -  apply(a[idx,],2,sum)
  
  return(list(shift.summary=shift.summary, shift.matrix=shift.matrix, new.r=new.r, new.maxsc = new.maxsc))
}

scenario2 <- function(required.servers, shift.setup, solver) {
  
  # Requirements
  if(shift.setup$period.days == 7) {
    r = shiftopt__required_matrix2(required.servers, shift.setup$period.stagger, shift.setup$period.days)
  } else {
    stop(sprintf('Periods of length %i currently not available', shift.setup$period.days))
  }
  
  # Available shifts
  sfts = shifts$shift_options(shift.setup$shift.types, shift.setup$period.stagger, shift.setup$period.days,
                              shift.setup$daily.sc.window)
  a = sfts$shifts
  cost = sfts$cost
  s = sfts$changeover
  maxsc = shift.setup$max.sc
  
  ns=dim(a)[1]
  np=dim(a)[2]
  
  # Do greedy fitting for each shift type
  sm = list()
  ss = list()
  i = 1
  maxsc = rep(shift.setup$max.sc, np)
  for(st in c('week_12hr', '4day_10.5hr')) {
    row.subset = sfts$type == st
    if(any(row.subset)) {
      soln = greedy.fitting(r, a[row.subset,], cost[row.subset], sfts$changeovers[row.subset,],
                     maxsc, solver)
      
      maxsc = soln[['new.maxsc']]
      r = soln[['new.r']]
      
      sm[[i]] = soln[['shift.matrix']]
      ss[[i]] = soln[['shift.summary']]
      
      i = i + 1
      
    }
    
  }
  
  # Fit remainder efficiently
  a_fun <- function(i, j) {
    combos <- CJ(i,j)
    ompr::colwise(combos[, a[V1,V2], by = seq_len(nrow(combos))]$V1)
  }
  
  s_fun <- function(i, j) {
    combos <- CJ(i,j)
    ompr::colwise(combos[, s[V1,V2], by = seq_len(nrow(combos))]$V1)
  }
  
  optm <- MILPModel() %>%
    add_variable(x[i], i = 1:ns, type = "integer") %>%
    set_objective(sum_expr(ompr::colwise(cost[i])*x[i], i = 1:ns), "min") %>%
    set_bounds(x[i], i=1:ns, lb = 0)
  
  for(j in 1:np) {
    optm = add_constraint(optm, sum_expr(a_fun(i, j)*x[i], i=1:ns) >= r[j])
    optm = add_constraint(optm, sum_expr(s_fun(i, j)*x[i], i=1:ns) <= maxsc[j])
  }
  
  #result = optm %>% solve_model(with_ROI(solver = solver))
  result = optm %>% solve_model(with_ROI(solver = solver, verbosity=1))
  
  soln = data.table(get_solution(result, x[i]))
  soln = soln[value != 0]
  
  ss[[i]] = soln[,.(
    type=rep(sfts$type[i], value),
    n=rep(value, value),
    cost=rep(sfts$costs[i], value)
  )]
  
  sm[[i]] = a[soln[,rep(i, value)],]
  
  return(list(shift.summary=rbindlist(ss), shift.matrix=do.call(rbind, sm)))                      
  
}