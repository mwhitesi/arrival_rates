# Name: shift_targets.R
# Desc: Pick shifts to meet demand
# Author: Matt Whiteside
# Date: Dec 11, 2018

optimumTarget <- function(required.servers, shift.setup, solver="glpk") {
  
  
  # Available shifts
  sfts = shifts$shift_options(shift.setup$shift.types, shift.setup$period.stagger, shift.setup$period.days,
                              shift.setup$daily.sc.window)
  a = sfts$shifts
  c = sfts$cost
  s = sfts$changeover
  
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
  
  a_fun <- function(i, j) {
    combos <- CJ(i,j)
    colwise(combos[, a[V1,V2], by = seq_len(nrow(combos))]$V1)
  }
  

  optm <- MILPModel() %>%
    add_variable(x[i], i = 1:ns, type = "integer") %>%
    set_objective(sum_expr(x[i]*colwise(c[i]), i = 1:ns), "min") %>%
    set_bounds(x[i], i=1:ns, lb = 0)
  
  # Required units constraint
  for(j in 1:np) {
    optm = add_constraint(optm, sum_expr(a_fun(i, j)*x[i], i=1:ns) >= r[j])
  }
  
  # Start time constraint
  max_starts = 2
  #optm = add_constraint(optm, sum_expr(*x[i]))
  
  
  
  result = optm %>% solve_model(with_ROI(solver = solver))
  
  soln = data.table(get_solution(result, x[i]))
  soln = soln[value != 0]
  
  shift.summary = soln[,.(start=seconds_to_period((sfts$shift.info$start[i])*period.stagger*60),
          type=sfts$shift.info$type[i],
          n=value,
          len=sfts$shift.info$costs[i]
          )]
  
  
  return(list(shifts=shift.summary, shift.periods=a[soln$i,]))
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
  
  stopifnot(all(r != 0)) # Always need a ambulance
  
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