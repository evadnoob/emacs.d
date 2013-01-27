;;-!*- mode: JDE; default-directory: "e:/khub/Perf20/java/" -*-
         sql =
            "SELECT " +
            "   c.category_id , " +
            "   cg.parent_category_id AS parent_category_id, " +
            "   c.category_name, " +
            "   tc.category_name AS parent_name, " +
            "   c.category_desc, c.owner_org_id, c.created_by, " +
            "   c.created_date, c.updated_by, c.updated_date, cat.product_type_id " +
            " FROM  " +
            "   (select category_id, product_type_id " +
            "      from accessible_categories_view " +
            "     where product_type_id != " +
            ProductManagementConstants.PRODUCT_TYPE_ID_BRAINBENCH_EXAM +
            "       and person_id = ? " +
            "	 group by category_id, product_type_id) cat,  " +
            "  category_group cg, category c, category tc " +
            " WHERE c.category_id = cg.child_category_id " +
            "  AND tc.category_id = cg.parent_category_id "+
            "  AND cat.category_id = cg.child_category_id "+
            "  and tc.category_id = ? " +
            " ORDER BY UPPER(tc.category_name), UPPER(c.category_name)";


      sql =
         "update object_bitset"
         + "  set is_stale = 1 "
         + "  where object_type_id = 2 /* person */"
         + "   and bitset_type_id = 1 /*offered*/"
         + "   and object_id in ("
         + "     select p.person_id "
         + "        from person p, org_hierarchy oh, offer_price op, org_collection oc"
         + "        where op.target_org_collection_id = oc.org_collection_id"
         + "       and oc.org_id = oh.ancestor_org_id"
         + "       and oh.org_id = p.org_id "
         + "       and trunc(op.price_end_date) between trunc(sysdate) -2 "
         + "       and trunc(sysdate) -1)";





      String sql =
         "SELECT "
         + " NVL(ps.series_name, 'More Related Products') series_name, "
         + " sp.product_id, "
         + " sp.product_name, "
         + " sp.product_type_id type_id, "
         + " sp.product_status_id status_id, "
         + " sp.is_subscribed, "
         + " sp.is_offered, "
         + " (SELECT PRODUCT_ATTRIBUTE_VALUE FROM PRODUCT_ATTRIBUTE_VALUE "
         + "   WHERE product_type_attribute_id = "+ ProductManagementConstants.ONLINE_TECH_REF_PTAID_ISBN
         + "     AND sp.product_type_id =  " + ProductManagementConstants.PRODUCT_TYPE_ID_ONLINE_TECH_REF
         + "     AND product_id = sp.product_id) isbn, "
         + " (SELECT PRODUCT_ATTRIBUTE_VALUE FROM PRODUCT_ATTRIBUTE_VALUE "
         + "   WHERE product_type_attribute_id = " + ProductManagementConstants.ONLINE_TECH_REF_PTAID_HOST_URL
         + "     AND sp.product_type_id =  " + ProductManagementConstants.PRODUCT_TYPE_ID_ONLINE_TECH_REF
         + "     AND product_id = sp.product_id) external_host "
         + " FROM "
         + "  (SELECT DISTINCT spv.product_id, spv.product_name, spv.product_type_id, spv.product_status_id,"
         + "               1 is_subscribed, 0 is_offered"
         + "          FROM subscribed_products_view spv,"
         + "               category_product_map cpm, category_group cg"
         + "         WHERE spv.person_id = ?"
         + "           AND spv.is_product_active = 1"
         + "           AND spv.product_type_id != " + ProductManagementConstants.PRODUCT_TYPE_ID_BRAINBENCH_EXAM
         + "           AND spv.product_id = cpm.product_id"
         + "           AND cpm.category_id = cg.child_category_id"
         + "           AND ? IN (cg.child_category_id, cg.parent_category_id)";




insertStmt = con.prepareStatement(
                  "insert into interest ( person_id, category_id, created_by, created_date ) " +
                  "select ?, ?, ?, SYSDATE from dual " +
                  "where not exists " +
                  "	  (select 'x' from interest where person_id = ? and category_id = ?)");

"select x,"
+ "   NVL(COUNT(ar.learning_object_id), 0) courses_completed_count, \n"
+ " from xxx ";



SELECT c.category_id
 c.category_name
 c.category_desc
 c.owner_org_id
 c.created_by
 c.created_date
 c.updated_by
 c.updated_date
 ' ' parent_name FROM category c 
  WHERE c.category_id IN (SELECT /*+ INDEX_FFS(cpm) */ DISTINCT cpm.category_id FROM (SELECT DISTINCT product_id
 product_vendor_id FROM subscribed_products_view WHERE person_id = ? AND is_product_active = 1 AND product_type_id <> 18 ) prod
 category_product_map cpm
 category_group cg 
WHERE cpm.product_id = prod.product_id 
AND cg.child_category_id= cpm.category_id ) 
AND c.owner_org_id = ? 
AND c.category_name IS NOT NULL 
AND UPPER(TRIM(c.category_name)) LIKE ? ORDER BY UPPER(c.category_name) ASC 




SELECT c.category_id, c.category_name, c.
category_desc, c.owner_org_id, c.created_by, c.created_date, c.updated_by, c.updated_date, ' ' parent_name FROM category c WHERE c.category_id IN (SELECT /*+
 INDEX_FFS(cpm) */ DISTINCT cpm.category_id FROM (SELECT DISTINCT product_id, product_vendor_id FROM subscribed_products_view WHERE person_id = ? AND is_prod
uct_active = 1 AND product_type_id <> 18 ) prod, category_product_map cpm, category_group cg WHERE cpm.product_id = prod.product_id AND cg.child_category_id
= cpm.category_id ) AND c.owner_org_id = ? AND c.category_name IS NOT NULL AND UPPER(TRIM(c.category_name)) LIKE ? ORDER BY UPPER(c.category_name) ASC 
