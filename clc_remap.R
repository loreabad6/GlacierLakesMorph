clc2018 <- read.table('CLC2018_CLC2018_V2018_20_QGIS.txt', sep = ',')
names(clc2018) <- c('code','r','g','b','a','desc')
clc2018$code <- as.factor(clc2018$code)

clcaoi <- read.table('clc2018_AOI.txt')
names(clcaoi) <- c('code','count')
clcaoi$code <- as.factor(stringr::str_remove(clcaoi$code, ':'))

clc <- dplyr::inner_join(clc2018, clcaoi) %>% dplyr::select(-c(r,g,b,a))
clc_remap <- clc %>% mutate(
  class = ifelse(startsWith(code, '1'), 'urban',
    ifelse(startsWith(code, '2'), 'agriculture',
      ifelse(startsWith(code, '31'), 'forest',
        ifelse(startsWith(code, '32'), 'shrubs',
          ifelse(startsWith(code, '331'), 'sands',
            ifelse(startsWith(code, '332'), 'rock',
              ifelse(startsWith(code, '333'), 'sparseveg',
                ifelse(startsWith(code, '335'), 'glacier',
                  ifelse(startsWith(code, '4'), 'wetlands', 'water'
                  )
                )
              )
            )
          )
        )
      )
    )
  )
) %>% mutate(
  recode = ifelse(
    startsWith(code, '1'), 1,
    ifelse(startsWith(code, '2'), 2,
      ifelse(startsWith(code, '31'), 3,
        ifelse(startsWith(code, '32'), 4,
          ifelse(startsWith(code, '331'), 5,
            ifelse(startsWith(code, '332'), 6,
              ifelse(startsWith(code, '333'), 7,
                ifelse(startsWith(code, '335'), 8,
                  ifelse(startsWith(code, '4'), 9, 0
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

## String to feed on GEE remap 
### from
paste(as.character(clc_remap$code), collapse = ', ')

### to
paste(as.character(clc_remap$recode), collapse = ', ')

clc_remap %>% group_by(class) %>% summarise(recode = first(recode)) %>% arrange(recode)