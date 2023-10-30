% http_server (better name TBD) - A HTTP server library for the Unison programming language
% Backend Street Boys team - Track T
% April 23, 2023

# Introduction
- Unison: an emerging language *especially* designed for distributed computing environment
- Very young languge, standard library is still lacking
- In particular, a solid HTTP library is still sorely needed

# Context
- Why Unison?
- Why an HTTP library?

# Why Unison?
- Highly exciting functional programming language inspired by Haskell, Erlang/Elixir and Smalltalk
- Gaining recognization and traction from both the academia and the industry's leaders
  - Dave Thomas, founder of PragProg *and* one of the authors of The Agile Manifesto very recently has a few blog post promoting Unison
- Some very innovative features
  - Immutable and content-addressable *code*
  - Distributed static type-safety
  - Everything serializable (code, data *and* thread of execution) and can be persisted or transported over the network
  - The first general purpose, industry-targeted language to implement an algebraic effect system (called Ability)
  - In the words of Dave Thomas, "Unison is a crazily innovative language"
=> Huge potential and still at the early stage of development
=> Early adopters and contributors are always the most benefited when a new, innovative and potentially disrupting technology comes into its exponential growth stage, and still have the edge for a long time after that

# Why an HTTP library? (1)
- Something the community and the ecosystem need
- A more infrastructural project, as a long term technical exercise for us backend boys to further develop their engineering skill
- And HTTP libraries are  something we use daily in our works -> already have an understanding and certainly will develop deeper appreciation of what we usually take for granted

# Why an HTTP library? (2)
- Also there are reasons to submit a library instead of a conventional business system:
  - Longer term investment and development to improve our complexity management, refactoring and software architecture skills
  - A level deeper into the so-called "tech stack", to open up the possibilities of what we can create
  - In-house open source contribution/giving back to the community is both satisfying *and* beneficial business-wise
  - Particularly for the Hackathon:
    - Backend library is absolutely sterse and leaves no room for unsubstantial fancies -> we have no choice but to purely focus on the technical side, which is the point of track T
    - The applicability of a HTTP library is huge, being the foundation of most comtemporary API backends

# Objectives and scope of the project
- Minimalistic and focus on what a HTTP server lib should do best: routing, request handling, concurrency control...
- Makes no assumption about the user's architecture -> stay a library, not a framework, not even a microframework!
- Stay away from the user's business logic
- Stay small and easily composable to other libs in the ecosystem to form a whole backend (template engine, JSON parser and renderer, logger, persistent layer, authen/author, other protocol server e.g. GRPC...)

# Features supported
- Simple routing algorithm:
  - Based on *parser combinator* instead of regex, since this is what Unison offers and is actually both more efficient and more ergonomic
- Supports all common HTTP methods and headers
- Handler are *composable*: independent handlers can be composed into a new router, instead of the common approaches where you have a router object, then register handler for an endpoint URI and method combo
- Effect-based request parameters instead of conventional context object-based approach
- Concurrency control and rate limiting using semaphores

# Features to be implemented
- A LCP-based routing algorithm for better performance:
  - High priority, often the make-or-break factor to consider when there are many competing alternatives
- Middleware system:
  - Actually really easy to implement in a functional programming language like Unison
  - But still left open to possibilities, as Abilities/Effects can also be used to effectively implement MW
- Form params in RequestParams ability
- Another Ability for ReponseRender

# Pitching points for track T
- Useability:
  - Somewhat different from normal HTTP server lib, but not wildly different from a backend developer's perspective

- Technical challenge:
  - Both familiar (HTTP server lib) and uncharted (library for a very new language) territory
  - More infrastructural than other competing project in track T
  - Make use of many unique features of Unison

- Applicability scope:
  - The scope is projects with HTTP-based/RESTful API for backend
  - The limitation is how widely adopted Unison's going to be

- Prototype quality: 
  - Most essential features are implemented
  - Some left out features can be implemented without issue
  - Still not production-ready quality
