draw_a_sample = function(n, 
                         curly_mean = 170, curly_sd = 7,
                         straight_mean = 165, straight_sd = 7){
    list(curly = rnorm(n, curly_mean, curly_sd), 
         straight = rnorm(n, straight_mean, straight_sd))
}

plot_a_sample = function(sample, mean_se = TRUE) {
    sample_df = as_tibble(sample) %>% 
        gather(key = 'group', value = 'height')
    
    plot = ggplot(sample_df, aes(group, height)) +
        geom_point()
    
    if (mean_se) {
        plot = plot + 
            stat_summary(fun.data = 'mean_cl_normal',
                         color = 'red', 
                         position = position_nudge(x = .1))
    }
    return(plot)
}

calculate_p_value = function(sample) {
    p.value = t.test(sample$curly, sample$straight)$p.value
    return(p.value)
}

power_fn = function(n, delta, sd) {
    power.t.test(n, delta, sd)$power
}

calculate_means = function(a_sample) {
    map(a_sample, mean)
}

replicate_N_times = function(N, n, alpha = .05, ...) {
    replications = tibble(rep_idx = 1:N, 
                          n = n) %>% 
        mutate(sample = map(n, draw_a_sample, ...), 
               straight = map(sample, 'straight'), 
               curly = map(sample, 'curly')) %>% 
        mutate(curlymean = map_dbl(straight, mean),
               curly.mean = map_dbl(curly, mean),
               variance = map2_dbl(straight, curly,
                                   ~ var(c(..1, ..2))),
               se = sqrt(variance/(2*n)),
               difference = curly.mean - curlymean,
               curly_greater = curly.mean > curlymean,
               t.test = map2(curly, straight, t.test),
               t.test.tidy = map(t.test, broom::tidy)) %>%
        unnest(t.test.tidy) %>%
        mutate(stat.sig = p.value < alpha)
    return(replications)
}


curly_greater_plot = function(replications) {
    replications %>% 
        count(curly_greater) %>% 
        mutate(share = n / sum(n)) %>% 
        ggplot(aes(curly_greater, share, fill = curly_greater)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(y = share + .05, 
                      label = scales::percent_format()(share))) +
        xlab('Is "curly" greater than "straight"?') +
        scale_y_continuous(labels = scales::percent_format()) +
        scale_fill_brewer(palette = 'Set1')
}

stat_sig_plot = function(replications) {
    replications %>% 
        count(stat.sig) %>% 
        mutate(share = n / sum(n)) %>% 
        ggplot(aes(stat.sig, share, fill = stat.sig)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(y = share + .05, 
                      label = scales::percent_format()(share))) +
        xlab('Is the difference between groups statistically significant?') +
        scale_y_continuous(labels = scales::percent_format()) +
        scale_fill_brewer(palette = 'Set1')
}


p_value_plot = function(replications) {
    ggplot(replications, aes(p.value)) +
        geom_density() +
        # geom_area(stat = 'density', 
        #           aes(fill = stat.sig, group = 1L)) +
        geom_rug() +
        xlab('p value') +
        geom_vline(xintercept = .05, linetype = 'dashed')
}

volcano_plot = function(replications, 
                       publication_bias = FALSE) {
    mean_effect = mean(replications$difference)
    mean_effect_bias = replications %>% 
        filter(stat.sig) %>% 
        pull(difference) %>% 
        mean()
    
    plot = ggplot(replications, aes(difference, -log10(p.value), color = stat.sig)) +
        geom_point() +
        geom_vline(xintercept = mean_effect, linetype = 'dashed') +
        scale_color_brewer(palette = 'Set1') +
        labs(x = 'difference (curly - straight)', 
             y = 'p-value (negative log scale)', 
             color = 'statistical\nsignificance')
    
    if (publication_bias) {
        plot = plot + 
            aes(alpha = stat.sig) +
            geom_vline(xintercept = mean_effect_bias, linetype = 'dashed', 
                       color = RColorBrewer::brewer.pal(3, 'Set1')[[2]]) +
            scale_alpha_discrete(guide = FALSE)
    }
    
    return(plot)
}

    
