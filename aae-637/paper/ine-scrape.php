<?php
/**	
 * 
 */
 
 
 // TOP LEVEL
 
  include_once("simple_html_dom.php");

  $curl_handle = curl_init ('http://www.ine.gob.bo/indice/poblacion92Proy.aspx?gestion=1992');
    
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($curl_handle, CURLOPT_FOLLOWLOCATION, true);

  $query_output = curl_exec($curl_handle);

  
  preg_match_all("|name=\"ctl00(.)ContentPlaceHolder1(.)ddlDepartamento(.*)/select|sU",
      $query_output,
      $dept_slice, PREG_PATTERN_ORDER);
      
  preg_match_all("|<option value=\"([0-9]*)\">|U",
      $dept_slice[0][0],
      $dept_vals, PREG_PATTERN_ORDER);
      
// DEPARTMENT LEVEL
      
  foreach ($dept_vals[1] as $dept_targ) {
  
    $curl_handle = curl_init ('http://www.ine.gob.bo/indice/poblacion92Proy.aspx?gestion=1992');
    
    curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($curl_handle, CURLOPT_FOLLOWLOCATION, true);

    $query_output = curl_exec($curl_handle);
  
    preg_match_all("|<input type=\"hidden\" name=\"__VIEWSTATE\" id=\"__VIEWSTATE\" value=\"(.*)\" />|U",
    $query_output,
    $state, PREG_PATTERN_ORDER);
        
    $curl_handle = curl_init ('http://www.ine.gob.bo/indice/poblacion92Proy.aspx?gestion=1992');
    
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($curl_handle, CURLOPT_FOLLOWLOCATION, true);

     $fields = array(
    'ctl00$ContentPlaceHolder1$ScriptManager1'=>'ctl00$ContentPlaceHolder1$UpdatePanel1|ctl00$ContentPlaceHolder1$ddlDepartamento',
    '__EVENTTARGET'=>'ctl00%24ContentPlaceHolder1%24ddlDepartamento',
    '__EVENTARGUMENT'=>'',
    '__LASTFOCUS'=>'',
    '__VIEWSTATE'=>$state[1][0],
    'ctl00$ContentPlaceHolder1$TipoBusqueda'=>'rbtnBusquedaGeografica',
    'ctl00$ContentPlaceHolder1$ddlDepartamento'=>$dept_targ,
    'ctl00$ContentPlaceHolder1$ddlProvincia'=>'-1',
    'ctl00$ContentPlaceHolder1$ddlSeccion'=>'-1',
    'ctl00$ContentPlaceHolder1$DDLCanton'=>'-1',
    'ctl00$ContentPlaceHolder1$DDLCiudad'=>'-1',
    'ctl00$ContentPlaceHolder1$DDL_PCiudad'=>'010101',
    'ctl00$ContentPlaceHolder1$RB_Rangos'=>'00'
  );
  
    curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $fields);

    $query_output = curl_exec($curl_handle);
  
    preg_match_all("|name=\"ctl00(.)ContentPlaceHolder1(.)ddlProvincia(.*)/select|sU",
      $query_output,
      $prov_slice, PREG_PATTERN_ORDER);
      
    preg_match_all("|<option value=\"([0-9]*)\">|U",
      $prov_slice[0][0],
      $prov_vals, PREG_PATTERN_ORDER);
      
// PROVINCE LEVEL            

    foreach ($prov_vals[1] as $prov_targ) {
    
    preg_match_all("|<input type=\"hidden\" name=\"__VIEWSTATE\" id=\"__VIEWSTATE\" value=\"(.*)\" />|U",
    $query_output,
    $state, PREG_PATTERN_ORDER);
        
    $curl_handle = curl_init ('http://www.ine.gob.bo/indice/poblacion92Proy.aspx?gestion=1992');
    
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($curl_handle, CURLOPT_FOLLOWLOCATION, true);

     $fields = array(
    'ctl00$ContentPlaceHolder1$ScriptManager1'=>'ctl00$ContentPlaceHolder1$UpdatePanel1|ctl00$ContentPlaceHolder1$ddlDepartamento',
    '__EVENTTARGET'=>'ctl00%24ContentPlaceHolder1%24ddlDepartamento',
    '__EVENTARGUMENT'=>'',
    '__LASTFOCUS'=>'',
    '__VIEWSTATE'=>$state[1][0],
    'ctl00$ContentPlaceHolder1$TipoBusqueda'=>'rbtnBusquedaGeografica',
    'ctl00$ContentPlaceHolder1$ddlDepartamento'=>$dept_targ,
    'ctl00$ContentPlaceHolder1$ddlProvincia'=>$prov_targ,
    'ctl00$ContentPlaceHolder1$ddlSeccion'=>'-1',
    'ctl00$ContentPlaceHolder1$DDLCanton'=>'-1',
    'ctl00$ContentPlaceHolder1$DDLCiudad'=>'-1',
    'ctl00$ContentPlaceHolder1$DDL_PCiudad'=>'010101',
    'ctl00$ContentPlaceHolder1$RB_Rangos'=>'00'
  );
  
    curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $fields);

    $query_output = curl_exec($curl_handle);
  
    preg_match_all("|name=\"ctl00(.)ContentPlaceHolder1(.)ddlSeccion(.*)/select|sU",
      $query_output,
      $sec_slice, PREG_PATTERN_ORDER);
      
    preg_match_all("|<option value=\"([0-9]*)\">|U",
      $sec_slice[0][0],
      $sec_vals, PREG_PATTERN_ORDER);
   
// SECCION LEVEL   
      
    foreach ($sec_vals[1] as $sec_targ) {
    
    preg_match_all("|<input type=\"hidden\" name=\"__VIEWSTATE\" id=\"__VIEWSTATE\" value=\"(.*)\" />|U",
    $query_output,
    $state, PREG_PATTERN_ORDER);
        
    $curl_handle = curl_init ('http://www.ine.gob.bo/indice/poblacion92Proy.aspx?gestion=1992');
    
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($curl_handle, CURLOPT_FOLLOWLOCATION, true);

     $fields = array(
    'ctl00$ContentPlaceHolder1$ScriptManager1'=>'ctl00$ContentPlaceHolder1$UpdatePanel1|ctl00$ContentPlaceHolder1$ddlDepartamento',
    '__EVENTTARGET'=>'ctl00%24ContentPlaceHolder1%24ddlDepartamento',
    '__EVENTARGUMENT'=>'',
    '__LASTFOCUS'=>'',
    '__VIEWSTATE'=>$state[1][0],
    'ctl00$ContentPlaceHolder1$TipoBusqueda'=>'rbtnBusquedaGeografica',
    'ctl00$ContentPlaceHolder1$ddlDepartamento'=>$dept_targ,
    'ctl00$ContentPlaceHolder1$ddlProvincia'=>$prov_targ,
    'ctl00$ContentPlaceHolder1$ddlSeccion'=>$sec_targ,
    'ctl00$ContentPlaceHolder1$DDLCanton'=>'-1',
    'ctl00$ContentPlaceHolder1$DDLCiudad'=>'-1',
    'ctl00$ContentPlaceHolder1$DDL_PCiudad'=>'010101',
    'ctl00$ContentPlaceHolder1$RB_Rangos'=>'00'
  );
  
    curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $fields);

    $query_output = curl_exec($curl_handle);
  
    preg_match_all("|name=\"ctl00(.)ContentPlaceHolder1(.)DDLCanton(.*)/select|sU",
      $query_output,
      $canton_slice, PREG_PATTERN_ORDER);
      
    preg_match_all("|<option value=\"([0-9]*)\">|U",
      $canton_slice[0][0],
      $canton_vals, PREG_PATTERN_ORDER);
      
// CANTON LEVEL
      
        foreach ($canton_vals[1] as $canton_targ) {
    
    preg_match_all("|<input type=\"hidden\" name=\"__VIEWSTATE\" id=\"__VIEWSTATE\" value=\"(.*)\" />|U",
    $query_output,
    $state, PREG_PATTERN_ORDER);
        
    $curl_handle = curl_init ('http://www.ine.gob.bo/indice/poblacion92Proy.aspx?gestion=1992');
    
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($curl_handle, CURLOPT_FOLLOWLOCATION, true);

     $fields = array(
    'ctl00$ContentPlaceHolder1$ScriptManager1'=>'ctl00$ContentPlaceHolder1$UpdatePanel1|ctl00$ContentPlaceHolder1$ddlDepartamento',
    '__EVENTTARGET'=>'ctl00%24ContentPlaceHolder1%24ddlDepartamento',
    '__EVENTARGUMENT'=>'',
    '__LASTFOCUS'=>'',
    '__VIEWSTATE'=>$state[1][0],
    'ctl00$ContentPlaceHolder1$TipoBusqueda'=>'rbtnBusquedaGeografica',
    'ctl00$ContentPlaceHolder1$ddlDepartamento'=>$dept_targ,
    'ctl00$ContentPlaceHolder1$ddlProvincia'=>$prov_targ,
    'ctl00$ContentPlaceHolder1$ddlSeccion'=>$sec_targ,
    'ctl00$ContentPlaceHolder1$DDLCanton'=>$canton_targ,
    'ctl00$ContentPlaceHolder1$DDLCiudad'=>'-1',
    'ctl00$ContentPlaceHolder1$DDL_PCiudad'=>'010101',
    'ctl00$ContentPlaceHolder1$RB_Rangos'=>'00'
  );
  
    curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $fields);

    $query_output = curl_exec($curl_handle);
  
    preg_match_all("|name=\"ctl00(.)ContentPlaceHolder1(.)DDLCiudad(.*)/select|sU",
      $query_output,
      $ciudad_slice, PREG_PATTERN_ORDER);
      
    preg_match_all("|<option value=\"([0-9]*)\">|U",
      $ciudad_slice[0][0],
      $ciudad_vals, PREG_PATTERN_ORDER);
    
// CUIDAD LEVEL  
      
    foreach ($ciudad_vals[1] as $ciudad_targ) {
    
    preg_match_all("|<input type=\"hidden\" name=\"__VIEWSTATE\" id=\"__VIEWSTATE\" value=\"(.*)\" />|U",
    $query_output,
    $state, PREG_PATTERN_ORDER);
        
    $curl_handle = curl_init ('http://www.ine.gob.bo/indice/poblacion92Proy.aspx?gestion=1992');
    
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($curl_handle, CURLOPT_FOLLOWLOCATION, true);

     $fields = array(
    'ctl00$ContentPlaceHolder1$ScriptManager1'=>'ctl00$ContentPlaceHolder1$UpdatePanel1|ctl00$ContentPlaceHolder1$ddlDepartamento',
    '__EVENTTARGET'=>'ctl00%24ContentPlaceHolder1%24ddlDepartamento',
    '__EVENTARGUMENT'=>'',
    '__LASTFOCUS'=>'',
    '__VIEWSTATE'=>$state[1][0],
    'ctl00$ContentPlaceHolder1$TipoBusqueda'=>'rbtnBusquedaGeografica',
    'ctl00$ContentPlaceHolder1$ddlDepartamento'=>$dept_targ,
    'ctl00$ContentPlaceHolder1$ddlProvincia'=>$prov_targ,
    'ctl00$ContentPlaceHolder1$ddlSeccion'=>$sec_targ,
    'ctl00$ContentPlaceHolder1$DDLCanton'=>$canton_targ,
    'ctl00$ContentPlaceHolder1$DDLCiudad'=>$ciudad_targ,
    'ctl00$ContentPlaceHolder1$DDL_PCiudad'=>'010101',
    'ctl00$ContentPlaceHolder1$RB_Rangos'=>'00'
  );
  
    curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $fields);

    $query_output = curl_exec($curl_handle);
    
    file_put_contents('ine_city_level.html', $query_output);
    
    preg_match_all("|<option selected=\"selected\" value=\"([0-9]*)\">(.*)</option>|U",
    $query_output,
    $final_output, PREG_PATTERN_ORDER);
    
    $fp = fopen('ine_numeric_geog_data.txt', 'a');
    fputcsv($fp, $final_output[1]);
    
    $fp = fopen('ine_names_geog_data.txt', 'a');
    fputcsv($fp, $final_output[2]);
    
    echo var_export($final_output[2]);
    
  
}
}
}
}
}



?>