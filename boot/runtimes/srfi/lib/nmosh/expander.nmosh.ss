(library (nmosh expander)
	 (export (rename (ex:expand-sequence expand-sequence) (ex:expand-sequence-r5rs expand-sequence-r5rs) (ex:environment expander-environment)))
	 (import (primitives ex:expand-sequence ex:expand-sequence-r5rs ex:environment)))
