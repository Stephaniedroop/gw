# Cut 05

# Concatenate 'o' + setting of K, KS, sem
path$set_o <- paste0('o', path$K, path$KS, as.numeric(path$sem))
path$set_u <- paste0(
  'u',
  path$Cu,
  path$Ku,
  path$Pu,
  path$Su,
  path$PKu,
  path$PCu,
  path$PSu_p,
  path$KCu,
  path$KSu,
  path$CSu
)

path <- path |>
  group_by(P, K, C, S, sem) |>
  mutate(condition = paste0('c', P, K, C, S, as.numeric(sem))) |>
  ungroup()
