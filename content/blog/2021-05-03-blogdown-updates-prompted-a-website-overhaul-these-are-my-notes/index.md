---
title: 'blogdown updates prompted a website overhaul: These are my notes'
author: A. Solomon Kurz
date: '2021-04-27'
draft: false
excerpt: "The purpose of this post is to highlight some of the steps I took to rebuild my academic-style blogdown website. At a minimum, I'm hoping this post will help me better understand how to set up my website the next time it needs an overhaul. Perhaps it will be of some help to you, too."
layout: single
tags:
- blogdown
- hugo
- Netlify
- R
- tutorial
lastmod: '2021-04-26T11:27:11-07:00'
featured: no
bibliography: /Users/solomonkurz/Dropbox/blogdown/content/post/my_blog.bib
biblio-style: apalike
csl: /Users/solomonkurz/Dropbox/blogdown/content/post/apa.csl  
link-citations: yes
---

## Purpose

A few weeks ago, I was preparing to release the second blog post in a two-part series (you can find that post [here](https://solomonkurz.netlify.app/post/2021-04-22-effect-sizes-for-experimental-trials-analyzed-with-multilevel-growth-models-two-of-two/)). During the editing process, I had rendered the files into HTML and tried posting the draft to my website. Everything looked fine except that the figures wouldn’t render. I hadn’t seen this behavior before and I figured it had to do with some software update. When I checked, the [**blogdown** package](https://CRAN.R-project.org/package=blogdown) ([Xie et al., 2017](#ref-xieBlogdown2017), [2021](#ref-R-blogdown)) had indeed recently updated. I’d also noticed the great [Alison Hill](https://alison.rbind.io) had recently posted a few blogs on **blogdown**-related topics, so I figured it was time for a refresh.

The purpose of this post is to highlight some of the steps I took to rebuild my website and recover my figure-rendering game. At a minimum, I’m hoping this post will help me better understand how to set up my website the next time it needs an overhaul. Perhaps it will be of some help to you, too.

### I don’t cover everything.

This post is not an exhaustive introduction to **blogdown**. For that, you have the ebook by Xie, Hill, and Thomas ([2017](#ref-xieBlogdown2017)), [*blogdown: Creating websites with R markdown*](https://bookdown.org/yihui/blogdown/). A difficulty with that book is the authors designed it to cover a broad range of applications\[^1\], which means there isn’t enough room to cover every special case. A further difficulty is **blogdown** is something of a high-level interface for an array of more-specific software solutions (e.g., [Netlify](https://www.netlify.com/), [Hugo](https://gohugo.io/)), each of which has its own quirks. I am not going to introduce these in any great detail, either. For those purposes, you have the relevant reference manuals and other documentations available on the web.

### This isn’t the only way.

There are any number of ways one could make an academic website via **blogdown**. Hill provided one workflow in her post, [*Up & running with blogdown in 2021*](https://alison.rbind.io/post/new-year-new-blogdown/), upon which I’ll be drawing heavily throughout this post. Some of my steps will follow a different order from hers, based on what seemed right, for me.

## Foundations

Hill organized her aforementioned blog post, [*Up & running with blogdown in 2021*](https://alison.rbind.io/post/new-year-new-blogdown/), as if one were building their **blogdown** website from scratch. After futzing around with different strategies, I recommend this approach even if you’ve had a **blogdown** website up and running for a while. If you haven’t updated your website recently, archive your old files and build the new one from the ground up. Here’s the first step:

### Step 1. GitHub.

Log on to your GitHub account and start a fresh repo. Name it something website-y. I named mine “blogdown”, which you can find at <https://github.com/ASKurz/blogdown/>. If you need a refresher on GitHub, let the great Jenny Bryan lead you, [here](http://happygitwithr.com/) ([Bryan et al., 2020](#ref-bryanHappyGitGitHub2020)).

### Step 2. RStudio projects.

Make a fresh RStudio project\[^2\] to go along with your fresh GitHub repo. Within RStudio, you can do this by clicking through `File > New Project > Version Control > Git`. Next, you’ll want to paste in the URL from your GitHub repo. If you haven’t done something like this, before, go back online to your repo and click the green button near the top that’s labeled “Clone or download.” A URL will appear in there. That’s what you’ll be pasting into your new RStudio project, which will connect it to your GitHub repo. Hill discussed this [here](https://alison.rbind.io/post/new-year-new-blogdown/#step-1-create-repo).

## **blogdown** mini launch

### Step 3. Make a default **blogdown** site.

If you haven’t already, install the current version of **blogdown** by executing `install.packages("blogdown")`. Restart **R**, if necessary. Now within a fresh session within your RStudio project, execute `blogdown::new_site(theme = "wowchemy/starter-academic")`\[^3\]. Over the next minute or two, you’ll see a handful of new files pop up in your project folder. In your console, you’ll probably notice the prompt: “Want to serve and preview the site now? (y/n)”. I recommend executing `y`, which will return a preview of your default **blogdown** website in the RStudio Viewer panel.

<div id="refs" class="references csl-bib-body hanging-indent" line-spacing="2">

<div id="ref-bryanHappyGitGitHub2020" class="csl-entry">

Bryan, J., the STAT 545 TAs, & Hester, J. (2020). *Happy Git and GitHub for the <span class="nocase">useR</span>*. <https://happygitwithr.com>

</div>

<div id="ref-R-blogdown" class="csl-entry">

Xie, Y., Dervieux, C., & Presmanes Hill, A. (2021). *<span class="nocase">blogdown</span>: Create blogs and websites with R Markdown* \[Manual\]. <https://CRAN.R-project.org/package=blogdown>

</div>

<div id="ref-xieBlogdown2017" class="csl-entry">

Xie, Y., Hill, A. P., & Thomas, A. (2017). *<span class="nocase">blogdown</span>: Creating websites with R markdown*. Chapman and Hall/CRC. <https://bookdown.org/yihui/blogdown/>

</div>

</div>
