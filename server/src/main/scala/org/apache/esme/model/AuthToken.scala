package org.apache.esme.model

/**
 * Copyright 2008-2009 WorldWide Conferencing, LLC
 * 
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import net.liftweb._
import mapper._
import http._
import util._
import common._

import org.apache.esme._
import lib._
import org.apache.esme.actor._
import external._

import java.util.Calendar
import java.util.Date
import scala.xml.{Text, Node, Elem => XmlElem}

object AuthToken extends AuthToken with LongKeyedMetaMapper[AuthToken] {
  // override def dbIndexes = Index(user, status) :: super.dbIndexes
  
     override def create: AuthToken = {
    val ap = super.create
    ap.createdDate(new Date())
    ap
  } 
}

class AuthToken extends LongKeyedMapper[AuthToken] {
  def getSingleton = AuthToken // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object user extends MappedLongForeignKey(this, User)
  object description extends MappedPoliteString(this, 64)
   //define createfields
  object createdDate extends MappedDateTime(this)
  

  
  object uniqueId extends MappedUniqueId(this, 32) {
    override def dbIndexed_? = true
    
  }
}

