library(tidyverse)
library(ComplexUpset)
library(ggplot2movies)
library(officer)
library(devEMF)
library(ragg)

# https://krassowski.github.io/complex-upset/articles/Examples_R.html
# https://github.com/krassowski/complex-upset


setwd("C:/Users/sjdevine/OneDrive - USCIS/R/ComplexUpset")


#//////////////////////////////////////////////////////////////////////////////////////////////


# get data
movies = as.data.frame(ggplot2movies::movies)
head(movies, 3)

genres = colnames(movies)[18:24]
genres

movies[genres] = movies[genres] == 1
t(head(movies[genres], 3))

movies[movies$mpaa == '', 'mpaa'] = NA
movies = na.omit(movies)

movies
movies <- as_tibble(movies)
movies %>% glimpse()


#//////////////////////////////////////////////////////////////////////////////////////////////


# plot
plot <- movies %>% upset(data = ., intersect = genres, name = 'genre', width_ratio = 0.1, min_size = 10, 
                 # set_sizes = FALSE, # note that set_sizes = FALSE works, but setting it to TRUE throws an error
                 n_intersections = 15,
                 min_degree = 1, 
                 max_degree = Inf)
plot

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(plot)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "plot.docx")


#//////////////////////////////////////////////////////////////////////////////////////////////


# formatting (seems to rely on passing ggplot arguments via a list of annotations)

# disable counts over intersection_size bars
upset(
        movies,
        genres,
        base_annotations=list(
                'Intersection size'=intersection_size(counts=FALSE)
        ),
        min_size=10,
        width_ratio=0.1
)

# add colored counts over intersection_size bars
upset(
        movies,
        genres,
        base_annotations=list(
                'Intersection size'=intersection_size(
                        text_colors=c(
                                on_background='brown', on_bar='yellow'
                        )
                )
                + annotate(
                        geom='text', x=Inf, y=Inf,
                        label=paste('Total:', nrow(movies)),
                        vjust=1, hjust=1
                )
                + ylab('Intersection size')
        ),
        min_size=10,
        width_ratio=0.1
)

# edit text style for intersection_size bars
upset(
        movies,
        genres,
        base_annotations=list(
                'Intersection size'=intersection_size(
                        text=list(
                                vjust=-0.1,
                                hjust=-0.1,
                                angle=45
                        )
                )
        ),
        min_size=10,
        width_ratio=0.1
)

# fill the bars
upset(
        movies,
        genres,
        base_annotations=list(
                'Intersection size'=intersection_size(
                        counts=FALSE,
                        mapping=aes(fill=mpaa)
                )
        ),
        width_ratio=0.1
)

# fill the bars w custom colors
upset(
        movies,
        genres,
        base_annotations=list(
                'Intersection size'=intersection_size(
                        counts=FALSE,
                        mapping=aes(fill=mpaa)
                ) + scale_fill_manual(values=c(
                        'R'='#E41A1C', 'PG'='#377EB8',
                        'PG-13'='#4DAF4A', 'NC-17'='#FF7F00'
                ))
        ),
        width_ratio=0.1
)

# adjust height_ratio
# Setting height_ratio=1 will cause the intersection matrix and the intersection size to have an equal height:
plot <- upset(
        movies,
        genres,
        height_ratio=3.5,
        width_ratio=0.1,
        set_sizes = FALSE
)
plot

# adjusting other ggplot formatting
upset(
        movies, genres, width_ratio=0.1,
        base_annotations = list(
                'Intersection size'=(
                        intersection_size()
                        + ylim(c(0, 700))
                        + theme(plot.background=element_rect(fill='#E5D3B3'))
                        + ylab('# observations in intersection') +
                        # note that coord_fixed will change bar plot, but not the space reserved for bar plot by upset()
                        coord_fixed(ratio = .001 / 1, clip = "off") 
                )
        ),
        min_size=10
)

# coloring groups
upset(
        movies, c("Action", "Comedy", "Drama"),
        width_ratio=0.2,
        group_by='sets',
        queries=list(
                upset_query(
                        intersect=c('Drama', 'Comedy'),
                        color='red',
                        fill='red',
                        only_components=c('intersections_matrix', 'Intersection size')
                ),
                upset_query(group='Drama', color='blue'),
                upset_query(group='Comedy', color='orange'),
                upset_query(group='Action', color='purple'),
                upset_query(set='Drama', fill='blue'),
                upset_query(set='Comedy', fill='orange'),
                upset_query(set='Action', fill='purple')
        )
)


#//////////////////////////////////////////////////////////////////////////////////////////////


# adjust intersection matrix
upset(
        movies, genres, name='genre', min_size=10,
        encode_sets=FALSE,  # for annotate() to select the set by name disable encoding
        matrix=(
                intersection_matrix(
                        geom=geom_point(
                                shape='square',
                                size=3.5
                        ),
                        segment=geom_segment(
                                linetype='dotted'
                        ),
                        outline_color=list(
                                active='darkorange3',
                                inactive='grey70'
                        )
                )
                + scale_color_manual(
                        values=c('TRUE'='orange', 'FALSE'='grey'),
                        labels=c('TRUE'='yes', 'FALSE'='no'),
                        breaks=c('TRUE', 'FALSE'),
                        name='Is intersection member?'
                )
                + scale_y_discrete(
                        position='right'
                )
                + annotate(
                        geom='text',
                        label='Look here →',
                        x='Comedy-Drama',
                        y='Drama',
                        size=5,
                        hjust=1
                )
        ),
        queries=list(
                upset_query(
                        intersect=c('Drama', 'Comedy'),
                        color='red',
                        fill='red',
                        only_components=c('intersections_matrix', 'Intersection size')
                )
        )
)

# combining multiple plots
# there is an option to either passing a list of ggplot specs, use ggplot directly (best option), or use upset_annotate()
upset(
        movies,
        genres,
        annotations = list(
                # 1st method - passing list:
                'Length'=list(
                        aes=aes(x=intersection, y=length),
                        # provide a list if you wish to add several geoms
                        geom=geom_boxplot(na.rm=TRUE)
                ),
                # 2nd method - using ggplot
                'Rating'=(
                        # note that aes(x=intersection) is supplied by default and can be skipped
                        ggplot(mapping=aes(y=rating))
                        # checkout ggbeeswarm::geom_quasirandom for better results!
                        + geom_jitter(aes(color=log10(votes)), na.rm=TRUE)
                        + geom_violin(alpha=0.5, na.rm=TRUE)
                ),
                # 3rd method - using `upset_annotate` shorthand
                'Budget'=upset_annotate('budget', geom_boxplot(na.rm=TRUE))
        ),
        min_size=10,
        width_ratio=0.1
)

#//////////////////////////////////////////////////////////////////////////////////////////////


# adjust aspect ratio using patchwork
# note the coord_fixed options will resize plots, but not the space reserved for them by upset()
plot <- upset(
        movies, genres, name='genre', min_size=10, set_sizes = FALSE, 
        # height_ratio = 1,
        encode_sets=FALSE,  # for annotate() to select the set by name disable encoding
        base_annotations = list(
                'Intersection size'=(
                        intersection_size()
                        # note that coord_fixed will change bar plot, but not the space reserved for bar plot by upset()
                        # coord_fixed(ratio = .01 / 1, clip = "off")
                        )),
        matrix=(
                intersection_matrix() 
                # coord_fixed(ratio = 1 / 1, clip = "off")
                )
)

plot

# use patchwork to resize plot
# note that the output of upset is just a patchwork with 1 col and 2 rows
output_plot <- plot + plot_layout(widths = c(1), heights = unit(c(6, 3), c('cm')))
output_plot


# //////////////////////////////////////////////////////////////////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(output_plot)
dev.off()

# add emf to word doc
read_docx() %>%
        body_add_img(src = filename, width = 6, height = 6) %>%
        print(target = "plot.docx")


#//////////////////////


# you can save via ggsave (but it's not the best way - emf is better)

# with device = agg_png
# note that you can change aspect ratio using width/height args, while using scaling arg to adjust text size and crowding etc
# but using a png looks ok in word, but very fuzzy when converted to pdf 
# note png does show up with "save as pdf" and "print to pdf", whereas emf only shows up with "print to pdf"
# note you can just copy/paste from png to word, no resizing necessary

# with device = emf, the res and scaling args can't be used (will throw error)

# https://www.tidyverse.org/blog/2020/08/taking-control-of-plot-scaling/
ggsave(plot = output_plot, 
       
       # filename = "test_plot.png", 
       # device = agg_png,
       
       filename = "test_plot.emf",
       device = emf,
       
       # traditional size chart
       # width = 10, height = 6, units = "cm", 
       # res = 300, scaling = .5
       
       # wider chart
       # width = 10, height = 3, units = "cm", 
       # res = 300, scaling = .3
       width = 10, height = 6, units = "cm", 
       
)


#//////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////


# venn diagram
abc_data = create_upset_abc_example()

abc_venn = (
        ggplot(arrange_venn(abc_data))
        + coord_fixed()
        + theme_void()
        + scale_color_venn_mix(abc_data)
)

(
        abc_venn
        + geom_venn_region(data=abc_data, alpha=0.05)
        + geom_point(aes(x=x, y=y, color=region), size=1)
        + geom_venn_circle(abc_data)
        + geom_venn_label_set(abc_data, aes(label=region))
        + geom_venn_label_region(
                abc_data, aes(label=size),
                outwards_adjust=1.75,
                position=position_nudge(y=0.2)
        )
        + scale_fill_venn_mix(abc_data, guide='none')
)
















