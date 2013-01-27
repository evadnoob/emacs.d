(require 'tempo)

 (defvar tempo-initial-pos nil
   "Initial position in template after expansion")

 (defadvice tempo-insert( around tempo-insert-pos act )
   "Define initial position."
   (if (eq element '~)
         (setq tempo-initial-pos (point-marker))
     ad-do-it))

 (defadvice tempo-insert-template( around tempo-insert-template-pos act )
   "Set initial position when defined. ChristophConrad"
   (setq tempo-initial-pos nil)
   ad-do-it
   (if tempo-initial-pos
       (progn
         (put template 'no-self-insert t)
         (goto-char tempo-initial-pos))
    (put template 'no-self-insert nil)))

(eval-after-load "jde"
   '(define-abbrev jde-mode-abbrev-table "sop" ""
      (tempo-define-template
       "sop-template"
       '("System.out.println(\"" ~ "\");"))))


(eval-after-load "jde"
   '(define-abbrev jde-mode-abbrev-table "sof" ""
      (tempo-define-template
       "sof-template"
       '("System.out.printf(\"" ~ "\");"))))


(eval-after-load "jde"
   '(define-abbrev jde-mode-abbrev-table "lgdg" ""
      (tempo-define-template
       "dbg-template"
       '("log.debug(\"" ~ "\");"))))

(eval-after-load "jde"
   '(define-abbrev jde-mode-abbrev-table "lgtr" ""
      (tempo-define-template
       "dbg-template"
       '("log.trace(\"" ~ "\");"))))


(eval-after-load "jde"
   '(define-abbrev jde-mode-abbrev-table "tnr" ""
      (tempo-define-template
       "tnr-template"
       '("throw new RuntimeException(e);"))))

(defun add-imports (imports-to-add)
  ""
  (save-excursion 
    (jde-import-insert-imports-into-buffer 
     ;;(list "import org.springframework.transaction.*" "import org.springframework.transaction.support.*" ))
     imports-to-add
     (jde-import-organize t)
     ;;(jde-import-kill-extra-imports)
    )))
  ;;(save-excursion 
  ;;  (jde-import-kill-extra-imports)))


(tempo-define-template
 "jdbc-method"
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
(defalias 'dmb-jdbc-method 'tempo-template-jdbc-method)



(tempo-define-template
 "junit-class-381"
 '((P "classname: " classname t)

"package <com.thomson>;
import java.util.*;

import junit.framework.*;


/**
 * 
 * 
 * @author " (user-full-name) "
 */
public class " (s classname) " extends TestCase {
   
   /** logger */
   private final static LogService log =
      LogServiceFactory.createLogService(" (s classname) ".class);

   public " (s classname) "(String name)  {
      super(name);
   }

   static public TestSuite suite() {
      TestSuite suite  = new TestSuite();
      suite.addTest(new " (s classname) "(\"testXXXX\"));
      return suite;
   }


   public void testXXXX () throws Exception  {
   }

   public static void main(String args[]) { 
      junit.textui.TestRunner.run(suite());
   }
}"))
(defalias 'dmb-junit-class381 'tempo-template-junit-class381)


(tempo-define-template
 "dmb-junit-class"
 '((P "classname: " classname t)

"package <com.thomson>;
import java.util.*;

import org.junit.*;
import org.apache.log4j.*;
import org.apache.log4j.xml.DOMConfigurator;

import static org.junit.Assert.*;


/**
 * 
 * 
 * @author " (user-full-name) "
 */
public class " (s classname) " {
   
   /** logger */
   private final static Logger log =
      Logger.getLogger(" (s classname) ".class);

   @Test public void testXXXX () throws Exception  {
   }

   public static void main(String args[]) { 
     DOMConfigurator.configure(" (s classname) ".class.getClassLoader().getResource(\"log4j.xml\"));
     org.junit.runner.JUnitCore.runClasses(" (s classname) ".class);
   }
}" (jde-mode) ))
(defalias 'dmb-junit-class 'tempo-template-dmb-junit-class)

(tempo-define-template
 "test-method"
 '("
   @Test public void testOne () throws Exception {

   }
"))
(defalias 'dmb-test-method 'tempo-template-test-method)


(tempo-define-template
 "dmb-testng-class"
 '((P "classname: " classname t)

"package <package here>;
import java.util.*;

import org.apache.commons.lang.builder.*;
import org.apache.log4j.*;
import org.apache.log4j.xml.DOMConfigurator;

import org.testng.annotations.*;

import static org.testng.Assert.*;


/**
 * 
 * 
 * @author " (user-full-name) "
 */
@Test (groups = {\"unit\"}) 
public class " (s classname) " {
   
   /** logger */
   private final static Logger log =
      Logger.getLogger(" (s classname) ".class);

   @Test public void testXyz () throws Exception  {
   }
   
}" (jde-mode) ))
(defalias 'dmb-testng-class 'tempo-template-dmb-testng-class)




(tempo-define-template
 "log4j-category"
 '((P "category: " category t)
"<category" > n
"  name=\"" (s category) "\"" > n
"  class=\"com.thomson.service.log.adapter.Logger\">" > n
  "<priority value=\"DEBUG\"/>" > n
"</category>" > n
> n
))
(defalias 'dmb-log4j 'tempo-template-log4j-category)

(tempo-define-template
"dmb-class-with-main-method"
 '((P "classname: " classname t)
"
/** javac " (s classname) ".java && java -cp \".\" " (s classname) " */

import java.util.*;

public class " (s classname) " {
   
   public static final void main(final String[] args) {
        
   }
}"
 (jde-mode)
 (save-buffer)))
(defalias 'dmb-class-with-main-method 'tempo-template-dmb-class-with-main-method)


(tempo-define-template
 "dmb-test1"
 '((P "Comment: " comment-line noinsert)
  n
  "// " (s comment-line) > n))

(tempo-define-template
 "dmb-try-catch"
 '((P "exception: " exception-name noinsert)
  & "try { " > n
  r> > n
   "}" > n
  "catch (" (s exception-name) " e) {" > n
  "   throw new RuntimeException(e);" > n
  "}" > n) )
(defalias 'dmb-try-catch 'tempo-template-dmb-try-catch)

(tempo-define-template
 "dmb-spring-do-in-transaction"
 '(
  &
;;;   (save-excursion 
;;;     (jde-import-insert-imports-into-buffer 
;;;      (list "import org.springframework.transaction.*" "import org.springframework.transaction.support.*" ))
;;;     (jde-import-organize t)
;;;     (jde-import-kill-extra-imports)
;;;     )
  (add-imports 
    (list "import org.springframework.transaction.*" 
    "import org.springframework.transaction.support.*"))
  
"transactionTemplate.execute(new TransactionCallback() {" > n
"   public Object doInTransaction( " > n
"      TransactionStatus transactionStatus) {" > n
          r> > n
"         return null;" > n
"      } " > n
"   });"  > n) )
(defalias 'dmb-spring-do-in-transaction 'tempo-template-dmb-do-in-transaction)




(tempo-define-template
 "dmb-to-string-method"
 '(
  & 
  (save-excursion 
    (jde-import-insert-imports-into-buffer 
    (list "org.apache.commons.lang.builder.ToStringStyle" "org.apache.commons.lang.builder.ToStringBuilder" ))
    (jde-import-organize t))

   "public String toString() {" > n
   "return ToStringBuilder.reflectionToString(" > n
   "this, " > n
   "ToStringStyle.MULTI_LINE_STYLE);" > n
   "}" >
   ) "tempo-template-dmb-to-string-method")
(defalias 'dmb-to-string-method 'tempo-template-dmb-to-string-method)


(tempo-define-template
 "dmb-to-string-shortprefix-method"
 '(
  & 
  (save-excursion
    (jde-import-insert-imports-into-buffer 
     (list "org.apache.commons.lang.builder.ToStringStyle" "org.apache.commons.lang.builder.ToStringBuilder" ))
    (jde-import-organize t))
  
   "public String toString() {" > n
   "return ToStringBuilder.reflectionToString(" > n
   "this, " > n
   "ToStringStyle.SHORT_PREFIX_STYLE);" > n
   "}" >
   ) "tempo-template-dmb-to-string-shortprefix-method")
(defalias 'dmb-to-string-shortprefix-method 'tempo-template-dmb-to-string-shortprefix-method)


(tempo-define-template
 "dmb-hashcode-method"
 '(
  & 
  (save-excursion
    (jde-import-insert-imports-into-buffer 
     (list "org.apache.commons.lang.builder.*" ))
    (jde-import-organize t))
  
   "public int hashCode() {" > n
   "   return new HashCodeBuilder(<prime1>, <prime2>)." > n
   "      append(relationshipId). " > n
   "      append(relatedCompanyId)." > n
   "      append(companyId)." > n
   "      toHashCode();" > n
   "}" >
   ) "tempo-template-dmb-hashcode-method")
(defalias 'dmb-hashcode-method 'tempo-template-dmb-hashcode-method)
 

(tempo-define-template
 "dmb-equals-method"
 '(
  & 
  (save-excursion
    (jde-import-insert-imports-into-buffer 
     (list "org.apache.commons.lang.builder.*" ))
    (jde-import-organize t))
  
   "public boolean equals(Object obj) {" > n
   "   return new EqualsBuilder(" > n
   "      .append(relationshipId) " > n
   "      .append(relatedCompanyId)" > n
   "      .append(companyId)" > n
   "      .toHashCode();" > n
   "}" >
   ) "tempo-template-dmb-hashcode-method")
(defalias 'dmb-hashcode-method 'tempo-template-dmb-hashcode-method)


(tempo-define-template
 "dmb-logger-2"
 '(
    (save-excursion 
      (jde-import-insert-imports-into-buffer 
       (list "org.apache.log4j.*" )))

    (jde-gen-insert-at-class-top)
    > n
    > n
   "/** logger */" > n
   "private final static Logger log = " > n
   "Logger.getLogger(" (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ".class);"  % >
  ))
(defalias 'dmb-logger-2 'tempo-template-dmb-logger-2)



(tempo-define-template
 "dmb-log4j-logger"
 '(
;;    (setq jde-import-auto-collapse-imports nil
;;          jde-import-auto-sort nil)
   
   (save-excursion
     (jde-import-insert-imports-into-buffer 
      (list "org.apache.log4j.Logger" )))
   
    (jde-gen-insert-at-class-top)
    > n
    > n
   "/** logger */" > n
   "private final static Logger log = " > n
   "Logger.getLogger(" (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ".class);"  % >
  ) )
(defalias 'dmb-log4j-logger 'tempo-template-dmb-log4j-logger)

(tempo-define-template
 "dmb-tx-command"
 '(
  & 
    (save-excursion 
      (jde-import-insert-imports-into-buffer 
       (list "com.thomson.util.transaction.command.*" ))
      (jde-import-organize t))

    "new AbstractTxCommand() {" > n
    "// this value is UNUSED because we're invoking runWithConfig()" > n
    "public String getName() { return null; }" > n
    "public Object run(TxContext ctx) {" > n
    "return null;" > n
    "}" > n
    "}.runWithConfig(TxConfiguration.Required);" >
) )
(defalias 'dmb-tx-command 'tempo-template-dmb-tx-command)


(tempo-define-template
 "dmb-struts-action-method"
 '(
  & 
  "public void listAll( " > n
  "ActionMapping mapping, " > n
  "ActionForm form, " > n
  "HttpServletRequest request, " > n
  "HttpServletResponse response)" > n
  "throws Exception {" > n
  "log.debug(\"listAll\");" > n
  " } " >
) )
(defalias 'dmb-struts-action-method 'tempo-template-dmb-struts-action-method)

(tempo-define-template
 "dmb-insert-java-util-list"
 '(
  & 
  "List<> xxx = new ArrayList<xxx>(); " > n

))
(defalias 'dmb-insert-java-util-list 'tempo-template-dmb-insert-java-util-list)



(tempo-define-template
 "dmb-thomson-class"
 '((P "classname: " classname t)

"package com.thomson.;
import java.util.*;

import org.apache.log4j.*;

/**
 * 
 * 
 * @author " (user-full-name) "
 */
public class " (s classname) " {
   
   /** logger */
   private final static Logger log =
      Logger.getLogger(" (s classname) ".class);


}" (jde-mode)))
(defalias 'dmb-thomson-class 'tempo-template-dmb-thomson-class)


(tempo-define-template
 "dmb-annotate-spring-component"
 '(
   (save-excursion
     (jde-import-insert-imports-into-buffer 
      (list "org.apache.log4j.*" )))
   
    (jde-gen-insert-at-class-top)
    > n
    > n
   "/** logger */" > n
   "private final static Logger log = " n>
   "Logger.getLogger(" (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ".class);"  % >
  ) ) 



(tempo-define-template
 "dmb-copyright-insert"
 '(
    "/* " > n
    " * Copyright " (ucs-insert ?\u00a9) " " (format-time-string "%Y") ": Thomson Reuters Global Resources. All Rights Reserved. " > n
    " * Proprietary and Confidential information of TRGR. Disclosure, " > n
    " * Use or Reproduction without the written authorization of TRGR is prohibited " > n
    " */"   % >
     ))



(tempo-define-template
"dmb-spring-jdbc-class-with-main-method"
 '((P "classname: " classname t)
"
/** javac -cp \"f:/devtools/spring-framework-2.5-rc2/dist/spring.jar\" " (s classname) ".java && java -cp \".;f:/devtools/spring-framework-2.5-rc2/dist/spring.jar\" " (s classname) " */

import java.util.*;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;


public class " (s classname) " {
   
   public static final void main(final String[] args) {

      DriverManagerDataSource dataSource = new DriverManagerDataSource();
      dataSource.setDriverClassName(\"oracle.jdbc.driver.OracleDriver\");
      dataSource.setUrl(\"jdbc:oracle:thin:@U0103223-XPA:1521:wca\");
      dataSource.setUsername(\"\");
      dataSource.setPassword(\"\");

      JdbcTemplate jdbc = new JdbcTemplate(dataSource);
      System.out.println(\"count: \" + jdbc.queryForInt(\"select count(*) from all_company_attributes\"));
     
        
   }
}"
 (jde-mode)
 (save-buffer)))


(require 'tempo-x)

(tempo-define-template
 "dmb-maven-dependency"
 '(
  &
  "  <dependency>
      <groupId></groupId>
      <artifactId></artifactId>
      <version></version>
      <scope>compile</scope>
    </dependency>"
 > n

))

(tempo-define-template 
 "dmb-jdbctemplate-rowcallbak-handler"
 '(
  &
  "    jdbc2.query(
         \"\"
         new Object[]{},
         new RowCallbackHandler() {
            public void processRow(ResultSet rs) throws SQLException {
            }
         }
      );" 
  > n
))


;;(eval-after-load "sql-interactive"
;;   '






(define-abbrev jde-mode-abbrev-table "suppwarn" ""
      (tempo-define-template
       "suppwarn-template"
       '("@SuppressWarnings(\"\")")))


(tempo-define-template
 "dmb-gwt-rpc-call"
 '(
   &
"service.method(" > n
"   new AsyncCallback<Void>() {" > n
"   public void onFailure(Throwable err) {" > n
""  > n
"   }" > n
"   " > n
"   public void onSuccess(Void result) {" > n
"   ServerWaitPopup.hide();" > n
"   }" > n
"   }" > n
");" > n
  ))


(tempo-define-template
 "dmb-gwt-click-handler"
 '(
   &
".addClickHandler(new ClickHandler() {" > n
"      public void onClick(ClickEvent event) {" > n
"      }" > n
"});" > n
  ))