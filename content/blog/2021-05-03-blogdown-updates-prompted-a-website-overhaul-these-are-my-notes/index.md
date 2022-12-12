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

### Step 4. `.gitignore`.

I don’t know that you have to do this right now, but a good early step is to make a `.gitignore` file. Following Hill ([here](https://alison.rbind.io/post/new-year-new-blogdown/#using-github)), you can do this by executing `file.edit(".gitignore")`. Then go ahead and enter this content to the file:

``` r
.Rproj.user
.Rhistory
.RData
.Ruserdata
.DS_Store
Thumbs.db 
/public/
/resources/
```

Once you save the changes, you might execute `blogdown::check_gitignore()` to confirm you’re okay.

If you haven’t done so, yet, this would be a good time to commit your changes and push them to GitHub. Again, if this is new to you, let Bryan et al. ([2020](#ref-bryanHappyGitGitHub2020)) lead you.

## Netlify

### Step 5. Sign up and deploy.

Hill recommended you both build and host your **blogdown** website on Neflify (see [here](https://alison.rbind.io/post/new-year-new-blogdown/#step-1-create-repo)). I’m not going to argue. Go to <https://www.netlify.com/> and either log in or sign up using your GitHub account. Even if you already have a Netlify account, I recommend making a new Netlify site with `New site from Git > Continuous Deployment: GitHub`. You’ll then need to select your fresh GitHub repo, from above, to connect it to Netlify. This may require you to follow the prompts to actually navigate to GitHub, enable the connection, there, and then follow back to Netlify. Once you’re back in Netlify, leave settings at their defaults and select `Deploy Site`. You should be directed to a page with a header called *Production deploys* somewhere in the middle of the screen. After a minute of two, Netlify will finish deploying your site.

### Step 6. Customize your Netlify subdomain.

When you create your new site, Netlify will have automatically generated a subdomain name following the form `random-word-12345`. You should be able to see this at the top of your screen. This subdomain name will be part of your web address. You’re at liberty to keep the default name, if you want. But you can customize your subdomain name by navigating to `Site settings > General > Site details`. Then click the gray button named `Change site name`. In the field, I renamed my subdomain to `solomonkurz`. As a result, my website is deployed at <https://solomonkurz.netlify.app/>. Once you save this change, your website should be available at your customized address almost instantly.

## Start customizing

### Step 7. `config.yaml`.

You should see a `config.yaml` file in the root directory of your RStudio project folder. Open it and update the info at the top. Here’s what I changed:

``` r
title: A. Solomon Kurz # Website name
baseurl: 'https://solomonkurz.netlify.app/'
copyright: '© A. Solomon Kurz (2021)' 
```

### Step 8. `netlify.toml`.

Next, check for a `netlify.toml` file in your root directory. You can also open or create it by executing `blogdown::config_netlify()`. Once the file’s open, make sure Hugo is installed and matched up by executing `blogdown::check_hugo()`. I needed to update my `netlify.toml` file to include the following:

``` r
[build.environment]
  HUGO_VERSION = "0.82.0"
```

If this seems scary, Hill discussed it in greater detail [here](https://alison.rbind.io/post/new-year-new-blogdown/#step-5-publish-site).

### Step 9. `.Rprofile`.

Still following Hill ([here](https://alison.rbind.io/post/new-year-new-blogdown/#step-4-create-content)), make an `.Rprofile` file by executing `blogdown::config_Rprofile()`. Then customize some of the settings within `options()`, as desired. Mine now read:

``` r
options(
  # to automatically serve the site on RStudio startup, set this option to TRUE
  blogdown.serve_site.startup = FALSE,
  # to disable knitting Rmd files on save, set this option to FALSE
  blogdown.knit.on_save = TRUE,
  # build .Rmd to .html (via Pandoc); to build to Markdown, set this option to 'markdown'
  blogdown.method = 'html',
  # fix Hugo version
  blogdown.hugo.version = "0.82.0",
  # These changes are based on Alison Hill's posts:
  # https://alison.rbind.io/post/2019-02-21-hugo-page-bundles/#project-specific-rprofile
  # and 
  # https://alison.rbind.io/post/new-year-new-blogdown/#step-4-create-content
  blogdown.author = "A. Solomon Kurz",
  blogdown.ext = ".Rmd",
  blogdown.subdir = "post",
  blogdown.yaml.empty = TRUE,
  blogdown.new_bundle = TRUE
)
```

In her post, Hill stressed it’s important to restart your RStudio session after you save any changes to `.Rprofile`. If you haven’t done so, in a while, this is probably a good time to commit and push your files to GitHub.

## Content

### Step 10. `content/authors/admin/`.

Open the `content/authors/admin/_index.md` file and edit the default information to match yours, as needed. You can find Hill’s [here](https://raw.githubusercontent.com/rbind/apreshill/master/content/authors/alison/_index.md) and mine [here](https://raw.githubusercontent.com/ASKurz/blogdown/main/content/authors/admin/_index.md).

Within the `content/authors/admin/` folder, switch out the `avatar.jpg` with the pic you’d like to use for your profile. As part of that process, you’ll rename your pic as `avatar.jpg`.

I ended up commenting out the `icon: envelope` sub section within the `social` section, which removed the envelope icon from the array below my profile pic.

#### Step 10.a. `config/_default/` folder.

I wanted to enable the Google-Scholar icon. To do so, you may have to make and adjust the `params.yaml` file within the `config/_default/` folder. First, execute `rstudioapi::navigateToFile("config/_default/params.toml")` to make the folder. Second, navigate to `config/_default/params.toml` and open the file. Third, set:

``` r
icon:
  pack:
    ai: true
```

For me, this section was at the bottom.

While in the `params.yaml` file, add your email to make the contact widget work. Edit contact info, as desired. For example, I commented out the phone number and address.

I like to use a lot of `\(\LaTeX\)` formulas in my blog posts. If you would like to use `\(\LaTeX\)` in your website, too, make sure to set `math: true` within the `params.toml` file.

#### Step 10.b. Include your CV.

If you look at the bottom of the `content/authors/admin/_index.md` file, you’ll see the code for a nice widget which will make it easy for people to download a copy of your résumé or CV. I changed that code to read:

The next step is to save an actual copy of your CV in the `static/media` folder. While you’re in there, you might delete the `demo_resume.pdf` and `boards.jpg` files.

#### Step 10.c. Delete the mysterious other guy.

There was a second subfolder within the `content/authors` folder, which was named with what looked like Chinese characters. I deleted that subfolder.

### Step 11. `content/home`.

The `.md` files in the `content/home` folder control the contents of the website home page. Each `.md` file is for a widget. To turn them off, insert the following somewhere in the file (I chose to put these in the top, to make them more visible):

``` r
# Activate this widget? true/false
active: false
```

The **blogdown** default settings are way to busy for my taste. To de-clutter my home page, I set `active: false` to the following files:

-   `accomplishments.md`,
-   `demo.md`,
-   `experience.md`,
-   `featured.md`,
-   `hero.md`,
-   `posts.md`,
-   `projects.md`,
-   `publications.md`,
-   `skills.md`,
-   `tags.md`,
-   `talks.md`, and
-   `/gallery/index.md`.

### Step 12. Add custom `content/` folders.

I wanted to add a few `content/` sections that were not a part of the **blogdown** defaults. The new additions were:

-   `content/bookdown/`, which included a listing of my ebooks;
-   `content/conflicts/`, which included a brief discussion of my conflicts of interest; and
-   `content/support/`, which listed a few ways others might support my work.

Since these sections were all quite simple, each subfolder only included an `_index.md` file. Each of those files contained a brief YAML\[^4\] metadata header and some [Markdown](https://rmarkdown.rstudio.com) prose.

### Step 13. Delete unwanted `content/` folders.

Though one doesn’t need to do this, I cleaned out the `content/` folder, a bit, by deleting the

-   `content/event/`,
-   `content/project/`, and
-   `content/slides/` folders.

### Step 14. Personalize `content/courses`.

I streamlined this section by deleting the `content/courses/example` subfolder and listing my prior courses in Markdown-based paragraph form within the `content/courses/_index.md` file.

### Step 15: `config/_default/menus.yaml`.

This will break up the current workflow, a bit. But at this point it made sense to me to adjust the menu settings for the navigation bar link, which appears at the top of the website. Just to keep them for reference, I commented out the default settings. My custom settings were as follows:

``` r
main:
  - name: Home
    url: '#about'
    weight: 1
  - name: Blog posts
    url: post/
    weight: 2
  - name: Books
    url: bookdown/
    weight: 3
  - name: Publications
    url: publication/
    weight: 4
  - name: Courses
    url: courses/
    weight: 5
  - name: Conflicts of interest
    url: /conflicts
    weight: 6
  - name: Support
    url: support/
    weight: 7
```

If you follow this workflow, I recommend committing these changes, pushing them to GitHub, and inspecting the current website appearance after deployed by Netlify. We’re not done yet, but this will give you a sense of where the website is headed.

Your millage may vary, but be warned: The changes in the next two sections ate up several hours, each.

### Step 16: `content/publication/`.

I’m not sure when it was added, but the publications widget was new, to me. You can get some pointers for this section in Gina Reynolds blog post, [*Creating an ‘Academic Themed’ website with blogdown and Hugo*](https://evamaerey.github.io/what_how_guides/academic_website_w_blogdown#step-14-add-your-publications-in-the-contentpublication-folder), which is itself a supplement to Dan Quintana’s nice [tweetorial](https://twitter.com/dsquintana/status/1139846569623281664).

{{% tweet user="dsquintana" id="1139846569623281664" %}}

If it’s new to you, too, I **strongly recommend** you follow Quintana’s [advice](https://twitter.com/dsquintana/status/1139846908065669120) and begin by opening the `index.md` file in one of the example subfolders (e.g., `content/publication/journal-article/`) and slowly switch out the default information to match up with one of your publications. I was overly ambitious and tried to learn by building a personal subfolder by scratch. It’s easy to lose track of your mistakes, this way, and I recommend you save yourself the unnecessary aggravation by following Quintana’s advice, instead.

Although the publications widget allows one to include a featured picture for each publication, I wasn’t interested. Thus, I deleted the `featured.jpg` files from the subfolders.

For my purposes, most of the work, in this section, was concentrated in the YAML metadata within the `index.md` files. Here are a few pointers:

-   Make sure to use the `publication_types` parameter. For journal articles, you set `- 2`. To learn more, go to <https://wowchemy.com/docs/content/publications/#command-line>.
-   Default to wrapping your titles and abstracts within `''` or `""` marks. Sometimes, you’ll get away without them. You’ll run into trouble if, say, you leave them out and your title includes a `:` mark.
-   Within the `authors` section, list yourself as `admin`. It’ll be okay if you don’t, but you’ll lose out on functionality. Try it both ways to see what I mean.
-   It’s fine to list dates in a simple `'2020-12-01'` format.
-   For the `publication` section, I preferred to simply list the name of the relevant journal. If desired, you can use Markdown syntax to italicize and so on.
-   Treat the `tags` like keywords.
-   If your article has a freely-available PDF that is not locked behind a paywall, you might include that link in the `url_pdf` section. If your article is not freely available, but you’d like it to be, you can still host it on your website and include a link in the `url_pdf` section. What I did was first make a `pdf` subfolder within the static folder (i.e., `static/pdf/`). Second, I saved PDFs of my paywalled papers in that `static/pdf/` folder, with each file named by author, year, and title (your naming system might vary\[^5\]). Third, I linked to the relevant file within the relevant project subfolder. For example, one of my first papers was published with Sarah Bowen in 2012. That paper is listed in my `content/publication/Bowen & Kurz (2012a)/` subfolder. Here’s how I linked to the PDF:

``` r
url_pdf: "pdf/Bowen & Kurz (2012) Between-session practice and therapeutic alliance as predictors of mindfulness after mindfulness-based relapse prevention.pdf"
```

-   If you’d like to link to a site that is not part of those obviously included in the examples (e.g., the [Open Science Framework](https://osf.io/)), you can insert a custom-named link, like this:

``` r
links:
  - name: OSF
    url: 'https://osf.io/fdywh/'
```

-   It can be helpful to see how more-experienced users set up their `project` folders. You can find Hill’s [here](https://github.com/rbind/apreshill/tree/master/content/publication) and Quintana’s [here](https://github.com/dsquintana/website/tree/master/content/publication).

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
