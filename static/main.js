// Generated by CoffeeScript 1.6.2
(function() {
  var index_page_view, packageRactive, tag_page_view, tagsRactive, tags_page_view;

  tagsRactive = function(data, root, params) {
    var tags;

    tags = new Ractive({
      el: '#tags',
      template: '#tags_template',
      data: {
        items: data,
        page: 1,
        root: root
      }
    });
    return tags.on({
      next: function(event) {
        var next_page;

        next_page = event.context.page + 1;
        return $.get("" + root + "/" + next_page, params, function(data) {
          var i, v, _i, _len;

          for (i = _i = 0, _len = data.length; _i < _len; i = ++_i) {
            v = data[i];
            event.context.items.push(v);
          }
          return event.context.page = next_page;
        });
      }
    });
  };

  packageRactive = function(data, root, params) {
    var packages;

    if (params == null) {
      params = null;
    }
    packages = new Ractive({
      el: '#packages',
      template: '#packages_template',
      data: {
        items: data,
        page: 1,
        root: root
      }
    });
    return packages.on({
      new_tag: function(event) {
        var pid, tag,
          _this = this;

        pid = event.context["package"].key;
        tag = $(event.node).val();
        return $.post("/packages/tags/new", JSON.stringify([pid, tag]), function(id) {
          if (id) {
            event.context.tags.push({
              id: id,
              name: tag,
              lock: false
            });
            return event.node.value = '';
          }
        });
      },
      delete_tag: function(event, index) {
        var pkg, ptid,
          _this = this;

        ptid = event.context.id;
        pkg = packages.data.items[event.index.i];
        return $.post("/packages/tags/delete", JSON.stringify([ptid]), function(resp) {
          if (resp === true) {
            return pkg.tags.splice(index, 1);
          }
        });
      },
      load_packages: function(event) {
        var next_page;

        next_page = event.context.page + 1;
        return $.get("" + root + "/" + next_page, params, function(data) {
          var i, v, _i, _len;

          for (i = _i = 0, _len = data.length; _i < _len; i = ++_i) {
            v = data[i];
            event.context.items.push(v);
          }
          return event.context.page = next_page;
        });
      },
      edit: function(event) {
        var path;

        path = event.keypath + '.edit';
        return this.set(path, !this.get(path));
      }
    });
  };

  index_page_view = function(data) {
    return packageRactive(data, '/latest');
  };

  tags_page_view = function(data) {
    var tags;

    return tags = tagsRactive(data, '/taglist');
  };

  tag_page_view = function(data) {
    var packages, tag;

    tag = new Ractive({
      el: '#tag_info',
      template: '#tag_template',
      data: data[0]
    });
    tag.on({
      edit: function(event) {
        return this.set('editing', true);
      },
      submit: function(event) {
        var id, synopsis,
          _this = this;

        id = event.context.key;
        synopsis = event.node.value;
        return $.post("/tag/synopsis", JSON.stringify([id, synopsis]), function(resp) {
          if (resp === true) {
            _this.set('value.synopsis', synopsis);
            return _this.set('editing', false);
          }
        });
      }
    });
    return packages = packageRactive(data[1], "/tags/" + data[0].value.name);
  };

  $(function() {
    var packages, tags;

    $.params = function(param_name) {
      return new RegExp("[\\?&]" + param_name + "=([^&#]*)").exec(window.location.href)[1];
    };
    if ($('#page_index').size() === 1) {
      return index_page_view(data);
    } else if ($('#page_taglist').size() === 1) {
      return tags_page_view(data);
    } else if ($('#page_tag').size() === 1) {
      return tag_page_view(data);
    } else if ($('#page_search_tags').size() === 1) {
      return tags = tagsRactive(data, '/search/tags', {
        keyword: $.params('keyword')
      });
    } else if ($('#page_search_packages').size() === 1) {
      return packages = packageRactive(data, '/search/packages', {
        keyword: $.params('keyword')
      });
    }
  });

}).call(this);
