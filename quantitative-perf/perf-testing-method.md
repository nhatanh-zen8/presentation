Method for performance testing
==============================

# Foundational concerns

## Type of performance test to implement
- Load testing: steadily increasing load until service/performance degradation to test for system's load limit and system's
  performance under expected load
- Stress testing: test system under load over the expected limit and over a long duration to test for system's
  robustness and recoverability during extreme situation

## Which part of the system should be the entry point for load tests?
- Since we're testing HTTP servers, that should be API endpoints, but for better use of resources we should choose
  some APIs instead of testing all of them. 
- Write-heavy APIs
- Read-heavy APIs
- Interactive (websocket endpoints)
- => each requires a separate kind of test

## What variables/metrics to measure, in respect to what changes, under which invariance? And why?
- Metrics:
  - Basic OS metrics should be fine at this stage:
    - %CPU of the process under test
    - CPU load average
    - VMstat (%memory usage, swapin/swapout)
    - IOstat
    - => enough to infer load endurance and reliability
  - Can also measure latency between request and response on client side
  - Other metrics that require telemetry: Out of scope
- In respect to: number of concurrent connections and concurrent requests
- Invariance: cloud instance's specs, kernel parameters (ubuntu's defaults for 90% of cases), stable, "morally perfect link" network
  - These should be documented in the test report to (1) provide a benchmark to extrapolate specs/configs for better performance and (2) make the test result as reproducible and verifiable as possible

## How do we infer performance from the data of the above metrics?
- CPU load average: ideally <= number of cores. Under heavy load should be ~ number of cores, too low CPU load actually indicates problems with processing IO bound operation. Avg load too high is also not desirable for obvious reasons.
- VMstat: usage should be < physical memory under expected loads. Watch out for when there are too many swapin and swapout (e.g. page faults) happen, most degradation of service cases are due to this
- IOstat: mostly look out for % iowait of a process. over 5% for a dozen seconds might indicate IO bottleneck
 
# Implementation concerns

## Load testing environment
- Provision a complete environment (backend + populated database, client instances)?
  - Most complex and costly approach, but provide total isolation
  - Probably need to combine Terraform and a configuration management system
- Using an already up dev/test environment, only provision client hosts?
  - Less expensive and complex, but could make dev/test environment unavailable for a while, which could hinder other team
    members' work
  - Only need to provision some ephemeral client instances. In most cases we'd only need one client instance.
- User data: should be auto generated then clean up after test if required (e.g. using dev/test environment)
- For load tests, backend and clients should be on the same VPN or even local network, to reduce random network latency and make the test result
  more reproducible. It's easier to extrapolate how a system would behave in real world imperfect networking environment from stable, reproducible 
  test results under (mostly) ideal conditions than the other way around

## Load/stress testing tools
- Runs on client instances to make requests to backend
- With the typical ~10Ks concurrent requests, usually just one efficient stress tester running on one machine is enough, no need for
  distributed orchestration of stress testers
- Some options:
  - Goose (Rust): very performant and customizable. Currently doesn't support websocket out of the box but *does* support
    distributed load testing.
    - https://github.com/tag1consulting/goose
  - Chaperon (Elixir): stable, full featured and have websocket support
    - https://github.com/polleverywhere/chaperon
  - K6 (impelemented in Go, but user's load testing code is in Javascript): full featured, integrated with Grafana, heavy weight
    - https://k6.io/docs/
  - Gatling (Scala): full featured and performant, but required JVM
    - https://github.com/gatling/gatling
  - wrk/rewrk: performant and lightweight, but not as customizable as the above options
    - https://github.com/lnx-search/rewrk
  - References:
    - https://github.com/aliesbelik/load-testing-toolkit
    - https://github.com/denji/awesome-http-benchmark
  - Roll our own stress tester: this is also very viable, but one must be extremely careful with resource management, since this is
  one particular application where we *don't* want to reuse connections *at scale*. These concerns are already taken care of by well-implemented
  stress testing libs.

## Load/stress testing scenario
- Application-specific
- Scenarios for both load testing and stress testing can stay the same. The difference only lies in load characteristics.

## Metrics gathering
- Server side: Linux's sysstat utilities are good enough for now.
- Client side: Request latency might be supported out of the box depends on the chosen load tester util/lib, but generally not a problem.
