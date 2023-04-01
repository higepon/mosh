(library (srfi :98 os-environment-variables)
         (export get-environment-variable get-environment-variables)
         (import (only (system) get-environment-variables get-environment-variable))
)
