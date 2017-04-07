publish-github:
	@git commit docs -m "Regenerated site."
	@git push origin master

publish-aws-all:
	@aws --profile=starship s3 cp docs/ s3://starship.tools/ --recursive

publish-aws:
	@for f in `git status|grep modified|awk '{print $$2}'|egrep '^docs/'` ; do \
		aws --profile=starship s3 \
			cp "$$f" s3://starship.tools/`echo $$f|sed -e 's/^docs\///'` ; \
	done
