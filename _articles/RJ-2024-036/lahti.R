# examples with hetu package
library(hetu)
x <- c("010101A0101", "111111-111C", "290201A010M")
hetu(x)

hetu("010101A0101", extract = "sex")
hetu("010101A0101", extract = "date")

hetu::pin_sex("010101A0101")
hetu::pin_date("010101A0101")

hetu::pin_age("010101A0101", date = "2004-02-01", timespan = "months")

hetu_diagnostic("290201A010M")

example_vector <- c("290201A010M", "280201A010M", "290301A010M", "290200A010M")
columns <- c("valid.p.num", "valid.ctrl.char", "correct.ctrl.char",
             "valid.date")
hetu_diagnostic(example_vector, extract = columns)

set.seed(125)
x <- rpin(n = 4, p.male = 0.25, p.temp = 1.0)
x
hetu::pin_ctrl(x)
hetu::pin_ctrl(x, allow.temp = TRUE)

bid_ctrl(c("0000000-0", "0000001-9"))
satu_ctrl("10000001N")

diagnostics <- hetu_diagnostic(example_vector)
summary(diagnostics)

# Examples with sweidnumbr package
library(sweidnumbr)
example_pin <- c("640823-3234", "6408233234", "19640823-3230")
example_pin <- as.pin(example_pin)
example_pin

is.pin(example_pin)

sweidnumbr::pin_ctrl(example_pin)

sweidnumbr::pin_sex(example_pin)
sweidnumbr::pin_birthplace(example_pin)
sweidnumbr::pin_age(example_pin)
sweidnumbr::pin_age(example_pin, date = "2000-01-01")

example_oin <- c("556000-4615", "232100-0156", "802002-4280")
oin_group(example_oin)

set.seed(125)
roin(3)
