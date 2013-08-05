# !! coffee -c {file_name}

tagsRactive = (data, root) ->
  tags = new Ractive
    el: '#tags'
    template: '#tags_template'
    data:
      items: data
      page: 1
      root: root

  tags.on
    next: (event) ->
      next_page = event.context.page + 1
      $.get "#{root}/#{next_page}", (data) ->
        for v,i in data
          event.context.items.push v
        event.context.page = next_page

packageRactive = (data, root)->
  packages = new Ractive
    el: '#packages'
    template: '#packages_template'
    data:
      items: data
      page: 1
      root: root

  packages.on
    new_tag: (event) ->
      pid = event.context.package.key
      tag = $(event.node).val()
      $.post "/packages/tags/new", JSON.stringify([pid,tag]), (id) =>
        if id
          event.context.tags.push
            id: id
            name: tag
            lock: false
          event.node.value = ''

    delete_tag: (event, index) ->
      ptid = event.context.id
      pkg = packages.data.items[event.index.i]
      $.post "/packages/tags/delete", JSON.stringify([ptid]), (resp) =>
        if resp == true
          pkg.tags.splice(index, 1)

    load_packages: (event) ->
      next_page = event.context.page + 1
      $.get "#{root}/#{next_page}", (data) ->
        for v,i in data
          event.context.items.push v
        event.context.page = next_page

    edit: (event) ->
      path = event.keypath + '.edit'
      @set(path, !@get(path))

# data[0]: recent_tags
# data[1]: tags
index_page_view = (data) ->
  packageRactive(data, '/latest')

tags_page_view = (data) ->
  # tag
  # ============================================================================
  tags = tagsRactive(data, '/taglist')

tag_page_view = (data) ->
  # tag
  # ============================================================================
  tag = new Ractive
    el: '#tag_info'
    template: '#tag_template'
    data: data[0]

  tag.on
    edit: (event) ->
      @set('editing', true)

    submit: (event) ->
      id = event.context.key
      synopsis = event.node.value
      $.post "/tag/synopsis", JSON.stringify([id, synopsis]), (resp) =>
        if resp == true
          @set 'value.synopsis', synopsis
          @set('editing', false)

  # packages
  # ============================================================================
  packages = packageRactive(data[1], "/tags/#{data[0].value.name}")

$ ->
  if $('#page_index').size() == 1
    index_page_view data
  else if $('#page_taglist').size() == 1
    tags_page_view data
  else if $('#page_tag').size() == 1
    tag_page_view data
  else if $('#page_search_tag').size() == 1
    tags = tagsRactive(data)
  else if $('#page_search_package').size() == 1
    packages = packageRactive(data)
