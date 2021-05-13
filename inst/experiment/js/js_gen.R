source("~/Dropbox (Sydney Uni)/Apples and oranges/thesis/src/packages.R")
source("~/Dropbox (Sydney Uni)/Apples and oranges/thesis/src/functions.R")
source("~/Dropbox (Sydney Uni)/Apples and oranges/thesis/src/project_gen/project_gen_aggregation.R")

png(filename = here("img","aggregated_multi.png"), units="px", width=600, height=600, res=108)
plotdist(x = dist_samuelson$outcomes, y = dist_samuelson$probs, xlab = "Outcome ($ million)")
dev.off()

png(filename = here("img","aggregated_multi_original.png"), units="px", width=600, height=600, res=108)
plotdist(x = dist_samuelson$outcomes, y = dist_samuelson$probs, xlab = "Outcome ($)")
dev.off()

png(filename = here("img","aggregated_multi_redelmeier.png"), units="px", width=600, height=600, res=108)
plotdist(x = dist_redelmeier$outcomes, y = dist_redelmeier$probs, xlab = "Outcome ($)")
dev.off()

png(filename = here("img","aggregated_rep.png"), units="px", width=600, height=600, res=108)
plotdist(x = dist_rep$outcomes, y = dist_rep$probs, xlab = "Outcome ($ million)")
dev.off()

## image preload
img_preload <- str_c("img/", list.files(here("img")))

vars <- list(projects = projects,
             question = question,
             options = options,
             required = required,
             single_prompt = single_prompt,
             multi_prompt = multi_prompt,
             aggregated_rep_prompt = aggregated_rep_prompt,
             aggregated_multi_prompt = aggregated_multi_prompt,
             button_response = button_response,
             instructions = instructions,
             preamble = preamble,
             end_message = end_message,
             project_question = project_question,
             attend = attend,
             reappraise = reappraise,
             welcome = welcome,
             awareness = awareness,
             contact = contact,
             projects_neg = projects_neg,
             demographics = demographics,
             proj_input = proj_input,
             check_trials = check_trials,
             check_input = check_input,
             img_preload = img_preload,
             recruitment = recruitment,
             proj_input_rep = proj_input_rep)

getjs(vars, here("js","input.js"))

