# Name: shift_targets.R
# Desc: Pick shifts to meet demand
# Author: Matt Whiteside
# Date: Dec 11, 2018

optimumTarget1 <- function(required.servers, shift.setup, solver="glpk") {
  
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
    r = required_matrix2(required.servers, shift.setup$period.stagger, shift.setup$period.days)
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
  
  # Add back the 24/7 shifts
  shift.matrix = rbind(a[soln[,rep(i, value)],], matrix(1,nrow=n247,ncol=np))
  shift.summary = rbind(shift.summary, 
                        data.table(type=rep('week_247', n247),
                                   n=n247,
                                   cost=rep(24*7, n247)
                                   )
                        )
  
  return(list(shifts=shift.summary, shift.matrix=shift.matrix))
  
}



required_matrix2 <- function(required.servers, period.stagger, period.days) {
  # Map the format output from modelTarget to the required matrix
  # modelTarget could change and this function will need to be updated
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

required_matrix1 <- function(required.servers, period.stagger, period.days) {
  # Map the format output from modelTarget to the required matrix
  # modelTarget could change and this function will need to be updated
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

experiment <- function(required.servers, solver="glpk") {
  
}

scenario1 <- function(required.servers, solver) {
  
}