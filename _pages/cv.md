---
layout: archive
title: "CV"
permalink: /cv/
paperurl: 'https://gganggang2.github.io/smparkedu/files/CV_Seongmin Park_260926.pdf'
author_profile: true
redirect_from:
  - /resume
---

{% include base_path %}

Education
======
* Ph.D in Quantitative Psychology, University of California, Los Angeles (UCLA), 2031 (expected)
* M.A. in Educational Measurement and Statistics, Korea University, 2026
* B.S. in Mathematics Education, Korea University, 2023

Publications
======
  <ul>{% for post in site.publications reversed %}
    {% include archive-single-cv.html %}
  {% endfor %}</ul>
  
Talks
======
  <ul>{% for post in site.talks reversed %}
    {% include archive-single-talk-cv.html  %}
  {% endfor %}</ul>
  
Teaching
======
  <ul>{% for post in site.teaching reversed %}
    {% include archive-single-cv.html %}
  {% endfor %}</ul>
  
