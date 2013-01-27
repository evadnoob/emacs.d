(require 'tempo)

(tempo-define-template
 "backbone-view2"
 '((P "Method Name: " methodname t)
"
   /**
    * 
    * @param 
    * @param params an array of parameters.
    * @return a java.util.List of java.lang.Longs, never null.
    */
   static public List " (s methodname) "() {
      LmnkConnection conn = null;
      List result = new ArrayList();
      try {
         conn = ConnectionFactory.createLmnkConnection();
         
         PreparedStatement stmt = (params == null? conn.prepareStatement(sql): conn.prepareStatement(sql, params));
         ResultSet rs = stmt.executeQuery();
         while (rs.next()) {
            result.add(new Long(rs.getLong(1)));
         }

         return result;
      }
      catch (SQLException e) {
         throw new RuntimeException(e);
      }
      finally {
         if (conn != null) conn.close();
      }
   }"))
;;(defalias 'dmb-template-backbone-view 'tempo-template-backbone-view)




(tempo-define-template
 "backbone-view"
 '((P "View Name: " viewname t)
"
  /** 
   * A view
   */
  var " (s viewname) " = Backbone.View.extend({
    initialize: function(options) {
    },
    events: {

    },
    render: function() {
      var that = this;

      that.$el.append(
        ''
        + ''
        + ''
      );

      return this;
    }
  });
"))
