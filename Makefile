
test:
	printf "devtools::test()\n" | R --no-save

tests: test

doc:
	printf "devtools::document()\n" | R --no-save

release: doc
	export NAME=`basename ${PWD}` && \
		echo "PROJECT: $$NAME" && \
		cd .. && \
		R CMD build $$NAME/ && \
		R CMD check $$NAME*.tar.gz && \
    rm -rf $$NAME.Rcheck && mv $$NAME*.tar.gz $$NAME/ && \
		printf "\n\nIN R RUN:\n   r = getOption(\"repos\"); r[\"CRAN\"] = \"http://cran.es.r-project.org\"; options(repos = r); devtools::release()\n" && \
		git add . && \
		git commit -m "Release `cat DESCRIPTION | grep Version: | sed 's/^[^0-9]*\(.*\)$/\1/g'`" && \
		git push origin main -f


install: doc
	printf "devtools::document()\\ndevtools::install()\\n\\nq()\\n" | R --no-save
