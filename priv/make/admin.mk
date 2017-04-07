publish-github:
	@git commit docs -m "Regenerated site."
	@git push origin master

publish-aws:
	aws --profile=starship s3 cp docs/ s3://starship.tools/ --recursive

