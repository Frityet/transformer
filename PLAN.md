# Transformer plan

## Objectives

- [ ] Working transformer
    - [ ] Matrix library
        - [ ] fp32
        - [ ] row-major, continuous layout.
        - [ ] shape operations
            - [ ] `reshape`
            - [ ] `transpose`
            - [ ] `squeeze`/`unsqueeze`
            - [ ] `concat`
        - [ ] indexing
            - [ ] `gather`
        - [ ] math primitives
            - [ ] `matmul`
            - [ ] `add`
            - [ ] `mul`
            - [ ] `bias_add`
        - [ ] reductions
            - [ ] `sum`
            - [ ] `mean`
            - [ ] `max` 
        - [ ] nonlinear operations
            - [ ] gaussian error linear unit (GELU)
                - activation function for reducing error
                - differences with ReLU
            - [ ] `exp`
            - [ ] `log`
        - [ ] norms
    - [ ] Tensor library
        - [ ] 
    - [ ] Simple text predictor (SmallLM)
        - [ ] Tokeniser
        - [ ] tok2vec
        - [ ] 

## Week 1

### Takeaways

- Build things as it comes, do not try and "tick boxes" one by one
- Focus on simplier things (linalg, calc) before getting into more complicated matrices.
- Just inference? or training too?
    - Focus on inference for now (decoder only)
- Build own matrix/tensor lib, switch to real one later
- Everyone please actually attend :(
- Keep a conservative scope
- Eventually switch to external libs for matrix, tensor, etc 

### To study

1. Backprop
2. Gradient descent
3. Matrix operations

### Deliverables

1. `matmul`
2. Tokeniser
