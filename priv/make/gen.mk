POSTS_BASE_DIR = $(ROOT_DIR)/posts
BLOG_BUILD_DIR = $(ROOT_DIR)/docs
ASSETS_DIR = $(BLOG_BUILD_DIR)/assets
THEME_DIR = priv/sass
INDEXER_DIR = priv/indexer
STATIC_ASSETS = priv/assets
IMAGE_SOURCES = $(STATIC_ASSETS)/images
ICON_SOURCES = $(STATIC_ASSETS)/icons
FONT_SOURCES = $(STATIC_ASSETS)/fonts
SEARCH_TOOL_SOURCES = $(STATIC_ASSETS)/tipuesearch
CSS_SOURCES = $(STATIC_ASSETS)/css
JS_SOURCES = $(STATIC_ASSETS)/js
GULP_DIST = $(THEME_DIR)/dist

GULP_CMD = gulp
GULP = cd $(THEME_DIR) && $(GULP_CMD); cd -
GULP_WATCH = cd $(THEME_DIR) && $(GULP_CMD) watch &
GULP_SETUP = cd $(THEME_DIR) && npm install gulp && npm install; cd -
GULP_DOCS = cd $(THEME_DIR) && $(GULP_CMD) docs; cd -

post: POST_DIR=$(POSTS_BASE_DIR)/$(shell date +%Y-%m/%d-%H%M%S)
post: FILENAME=$(POST_DIR)/content.rfc822
post:
	@echo
	@echo "Creating blank post: $(FILENAME) ..."
	@mkdir -p $(POST_DIR)
	@echo "Subject: REQUIRED (title)" > $(FILENAME)
	@echo "Subtitle: " >> $(FILENAME)
	@echo "Excerpt: " >> $(FILENAME)
	@echo "From: REQUIRED (author)" >> $(FILENAME)
	@echo "Category: " >> $(FILENAME)
	@echo "Keywords: " >> $(FILENAME)
	@echo "Comments: " >> $(FILENAME)
	@echo "Content-Type: md" >> $(FILENAME)
	@echo "" >> $(FILENAME)
	@echo "[content goes here]" >> $(FILENAME)
	@echo
	@echo "Done."
	@echo

gulp-setup:
	@$(GULP_SETUP)

assets-clean:
	@rm -rf $(ASSETS_DIR)/*
	@mkdir -p $(ASSETS_DIR)/js $(ASSETS_DIR)/css

assets: assets-clean css
	@cp -r $(IMAGE_SOURCES) $(ASSETS_DIR)/
	@cp -r $(ICON_SOURCES) $(ASSETS_DIR)/
	@cp -r $(FONT_SOURCES) $(ASSETS_DIR)/
	@cp -r $(SEARCH_TOOL_SOURCES) $(ASSETS_DIR)/
	@cp -r $(CSS_SOURCES)/* $(ASSETS_DIR)/css/
	@cp -r $(JS_SOURCES)/* $(ASSETS_DIR)/js/
	@cp $(GULP_DIST)/*.css $(ASSETS_DIR)/css/
	@cp $(GULP_DIST)/*.js $(ASSETS_DIR)/js/

css:
	@echo "\nGenerating minimized and regular versions of CSS files ..."
	@echo
	@$(GULP)
	@echo "Done.\n"

css-watch:
	@$(GULP_WATCH) \

css-unwatch:
	@killall sass

theme-docs:
	@$(GULP_DOCS)

blog-header:
	@echo "\nBuilding blog ..."
	@echo

blog-html-only: blog-header clean compile
	@ERL_LIBS=$(ERL_LIBS) $(LFE) -e '(blog-cli:gen)'

index:
	cp $(INDEXER_DIR)/indexer.js $(BLOG_BUILD_DIR)
	cd $(BLOG_BUILD_DIR) && node indexer.js
	rm $(BLOG_BUILD_DIR)/indexer.js

blog: blog-html-only assets

serve-header:
	@echo "\nRunning blog server on http://$(LOCAL_DOCS_HOST):$(LOCAL_DOCS_PORT) ... (To quit, hit ^c twice)"
	@echo

serve-only:
	@ERL_LIBS=$(ERL_LIBS) erl -s blog-cli start-httpd -noshell

serve: clean compile serve-header serve-only

serve-watch-css: serve-header css-watch serve-only

serve-dev: blog-header clean compile assets serve-header
	@ERL_LIBS=$(ERL_LIBS) $(LFE) -s blog-cli gen-httpd

serve-dev-watch: blog-header clean compile assets css-watch serve-header
	@ERL_LIBS=$(ERL_LIBS) $(LFE) -s blog-cli gen-watch

.PHONY: docs
