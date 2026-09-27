---
layout: archive
title: "CV"
permalink: /cv/
author_profile: true
redirect_from:
  - /resume
---

{% include base_path %}

<p><a href="{{ site.baseurl }}/files/cv.pdf" class="btn btn--primary" target="_blank">Download Full CV (PDF)</a></p>

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
  
