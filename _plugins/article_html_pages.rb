module Jekyll

  class CategoryArticle < Page
    def initialize(site, base, dir, title, author, pages, issue, pdf)
      @site = site
      @base = base
      @dir = dir
      @name = 'index.html'

      self.process(@name)
      self.read_yaml(File.join(base, '_layouts'), 'article.html')
      ## Add the metadata to the page yaml, so it is
      ## available to liquid calls in article.html template
      self.data['title'] = title 
      self.data['author'] = author 
      self.data['pages'] = pages 
      self.data['issue'] = issue 
      self.data['pdf'] = pdf 
    end
  end

  class CategoryArticleGenerator < Generator
    safe true

    def generate(site)
      dir = "archive"
      site.config['issues'].each do |issue|
        
        if issue['issue'] != 'accepted'
          issue['articles'].each do |article|
            slug = article['slug']
            if(!slug.nil?)
              path = File.join(dir, issue['issue'], slug)
#              puts path
              pdf = path + ".pdf" 
              site.pages << CategoryArticle.new(site, site.source, path, article['title'], article['author'], article['pages'], issue['issue'], pdf)
            end
          end
        end
      end
    end
  end

end
