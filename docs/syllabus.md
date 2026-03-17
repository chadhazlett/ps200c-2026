---
layout: default
title: Syllabus
---

# PS200C: Causal Inference for Social Scientists — Spring 2026

*Class:* M 2-3:15 (Pub Aff 2238); W 2-3:15 (Bunche 3178)
*Section:* Friday 9:00–9:50 AM; see course website for location.

**Professor:** Chad Hazlett — [chazlett@ucla.edu](mailto:chazlett@ucla.edu)
*Office Hours:* Thursday 10am and by appointment, Bunche 3264

**Teaching Assistant:** Barney — [barneychen@ucla.edu](mailto:barneychen@ucla.edu)
*Office Hours:* See course website

---

## Why causal inference?

Much of what social scientists study are causal questions. Does foreign aid promote economic growth? Do elections reduce corruption? Does media exposure shift political attitudes? Yet the gap between the causal questions we ask and what the data can tell us is vast. Bridging that gap—carefully, honestly, and with the right tools—is what this course is about.

We begin by establishing a framework for thinking precisely about causal claims: what do we mean when we say *X* causes *Y*, and what would we need to know—or assume—to support such a claim? With that foundation in place, we move quickly to the identification strategies that form the backbone of modern empirical social science: randomized experiments, selection on observables, difference-in-differences, instrumental variables, regression discontinuity, and sensitivity analysis. Along the way, we will also cover directed acyclic graphs (DAGs) as a complementary language for expressing and evaluating causal assumptions.

## Approach for 2026

This year's course reflects a deliberate shift in how we think about graduate methods training in an era where AI tools can handle much of the mechanical work—estimation, coding, debugging—that used to dominate students' time outside of class.

Rather than assigning traditional problem sets, we are investing that time where it matters most: *applying causal reasoning to real research problems*. Starting early in the quarter, you will develop a final project in which you formulate a causal question, identify an appropriate strategy, and carry out an empirical analysis. The course is structured around milestones that build toward this project, with regular feedback from the instructor, TA, and peers.

In lecture, the emphasis is on building intuition for the core ideas and assumptions behind each identification strategy—not on derivations or code. You are expected and encouraged to use AI tools (Claude, ChatGPT, etc.) as tutors to fill in details, clarify concepts, and work through implementation. We want you to learn to *think* about causal inference; the tools can help you *do* it.

To keep everyone engaged and to create an honest signal of understanding, most class sessions will end with a short, pen-and-paper in-class activity reviewing the material covered that day. These replace exams as the primary individual assessment and also serve as the attendance mechanism.

## Course Requirements and Grading

### 1. In-class activities (20%)

Most class sessions will include a short (5–10 minute) pen-and-paper activity at the end of lecture. These are designed to reinforce the day's key ideas and give you immediate feedback on your understanding. They are graded primarily on completion and good-faith effort. Your lowest two scores will be dropped to accommodate absences.

### 2. Final project milestones (30%)

The final project is developed incrementally through a series of milestones. Each milestone is an opportunity for feedback and course-correction. The milestones are:

- **Milestone 1: Topic and group formation** (Week 3). Form a group of 1–3 people and submit a short paragraph describing your causal question and why it matters. You do not need to have a strategy or data yet.
- **Milestone 2: Identification strategy proposal** (Week 5). Describe the identification strategy you plan to use and why it is appropriate for your question. Discuss the key assumptions and what data you would need. It is fine if you are still choosing between strategies at this point.
- **Milestone 3: Data and preliminary analysis** (Week 8). Present your data, show descriptive statistics, and provide a preliminary attempt at your main analysis. Discuss what is and isn't working.

### 3. Final project (50%)

A research paper (roughly 10–15 pages) presenting your causal question, identification strategy, data, analysis, and interpretation. You should be honest about the limitations of your approach and discuss what assumptions are most vulnerable. Final projects are due during finals week. Groups will give brief presentations in the last week of class.

### Diagnostic self-assessment

In the first week, we will distribute a short self-diagnostic exercise (similar to the "Problem Set 0" from prior years). This is **ungraded**. Its purpose is to help you identify gaps in your background—probability, statistics, regression—so you can address them early. The course website's [Resources](resources.html) page will include curated materials keyed to each topic in the diagnostic. Section in the first two weeks will also cover key prerequisites.

### Using AI as a learning tool

You are expected to use AI tools throughout this course—not as a shortcut, but as a tutor. When something from lecture is unclear, try asking Claude or ChatGPT to explain it a different way, work through an example, or connect it to something you already know. This is a skill worth developing: learning to ask good questions of an AI is closely related to learning to think clearly about a problem.

To encourage this, each project milestone should include a brief **AI learning log**: one or two exchanges where you used an AI tool to clarify a concept or work through a problem related to the course, along with a sentence or two about what you learned. This is not assessed for quality—it is meant to normalize the practice and give us insight into what topics are causing confusion.

For the final project, you may use AI tools freely for coding, data wrangling, and exploring ideas. Your paper should include a short section describing how you used AI in the research process. There is no penalty for heavy use—the goal is reflective, honest engagement.

What AI tools *cannot* do for you is replace the hard thinking at the core of this course: choosing an identification strategy, evaluating whether its assumptions are credible in your setting, and interpreting what the results do and do not tell you. That is your job.

### Section

Section will serve two roles this year. First, Barney will use section to review prerequisite and supplementary material that we defer from lecture—this is where you will get more careful treatment of technical details, estimation, and coding. Second, section will include regular project check-ins: time to discuss your project with Barney and with other groups, get feedback, and troubleshoot.

### Attendance

Attendance is expected and tracked through the in-class activities. There is no separate attendance grade, but consistent absence will be reflected in your activity scores. If you need to miss class for a legitimate reason, let us know in advance.

## Textbooks

*The book you should buy is:*

- Angrist, Joshua D. and Jörn-Steffen Pischke. 2009. *Mostly Harmless Econometrics: An Empiricist's Companion*. Princeton University Press.

Another classic on the topic that is nice to have is:

- Morgan, Stephen L. and Christopher Winship. 2007. *Counterfactuals and Causal Inference: Methods and Principles for Social Research*. Cambridge University Press.

We will also make reference to "Causal Inference: What if" by Hernan and Robins, freely available [here](https://www.hsph.harvard.edu/miguel-hernan/wp-content/uploads/sites/1268/2024/01/hernanrobins_WhatIf_2jan24.pdf).

**A great review:** If you want to review statistics up through regression from a perspective that is very consistent with how we will use these concepts, check out chapters 1–3 of:

- Aronow, Peter and Benjamin Miller. *Foundations of Agnostic Statistics*. Available electronically from the [UCLA library](https://www.cambridge.org/core/books/foundations-of-agnostic-statistics/684756357E7E9B3DFF0A8157FB2DCECA).

A more comprehensive and annotated set of resources—including readings keyed to specific topics and the self-diagnostic—is available on the [Resources](resources.html) page.

## Topics

The following is an approximate schedule of topics. They do not correspond exactly to weeks, and the order or selection may shift. Detailed readings for each topic will be posted on the course website.

### 1. Introduction and the causal inference problem
- Why causal questions are hard; correlation vs. causation
- Overview of the course and its approach

### 2. Potential outcomes and the identification problem
- Counterfactual responses and the fundamental problem of causal inference
- Estimands: ATE, ATT, CATE
- Selection bias and why naïve comparisons fail

### 3. Randomized experiments
- Why randomization solves the identification problem
- Design: blocking, clustering, power
- Threats to validity: compliance, attrition, spillovers
- Natural experiments and the standard they set

### 4. Selection on observables
- Conditional ignorability / unconfoundedness
- Subclassification, matching, and weighting as strategies for balance
- The propensity score: theory and role
- Regression as a conditional-means machine
- When is selection on observables credible?

### 5. DAGs and structural causal models
- Directed acyclic graphs as a language for causal assumptions
- d-separation, backdoor criterion, front-door criterion
- DAGs as a visual language for the assumptions behind SOO, experiments, and what comes next

### 6. Sensitivity analysis
- What if unconfoundedness doesn't hold?
- Omitted variable bias and its formalization
- Sensitivity analysis via `sensemakr`: bounding the effect of unobserved confounders

### 7. Difference-in-differences
- The parallel trends assumption
- Two-period and staggered designs
- Falsification and pre-trend tests
- Panel data and fixed effects

### 8. Instrumental variables
- The logic of instruments: exogenous variation in treatment
- Exclusion restriction, relevance, monotonicity
- LATE and compliers
- Weak instruments and the problem of over-identification

### 9. Regression discontinuity
- Sharp and fuzzy designs
- Continuity assumptions and local identification
- Estimation, bandwidth, and specification checks
- Sorting and manipulation tests

### 10. Special topics (time permitting)

**(i) Panel data without parallel trends.**
- Synthetic control methods: constructing a counterfactual from weighted donors
- A new approach: when parallel trends is too strong, what can we assume instead?

**(ii) Safe inference, partial identification, and new opportunities.**
- Moving from making claims we can't support to describing what we can believe
- The SCQE (Stability-Controlled Quasi-Experiment): when treatment prevalence shifts sharply, partial identification by reasoning about how much outcomes could have changed absent the treatment shift — a quantity investigators can actually assess

**(iii) Mediation (or not) and the TRACE.**
- The temptation to condition on post-treatment variables — implementation checks, attention screens, mediators — and why it fails
- The TRACE (Treatment Reactive Average Causal Effect): bounded inference about effects within subgroups defined by post-treatment response, without the untenable assumptions of traditional mediation analysis

## Course website

All materials—slides, resources, project guidelines, and section materials—will be posted at:

<https://chadhazlett.github.io/ps200c-2026/>

Time-sensitive announcements will be posted through the UCLA course site (BruinLearn).
