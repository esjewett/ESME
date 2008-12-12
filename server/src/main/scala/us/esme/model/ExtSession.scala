package us.esme.model
/*
 * Copyright 2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */


import net.liftweb._
import mapper._
import util._

object ExtSession extends ExtSession with MetaProtoExtendedSession[ExtSession] {
  override def dbTableName = "ext_session" // define the DB table name

  def logUserIdIn(uid: String): Unit = User.logUserIdIn(uid)

  def recoverUserId: Can[String] = User.currentUserId

  type UserType = User
}

class ExtSession extends ProtoExtendedSession[ExtSession] {
  def getSingleton = ExtSession // what's the "meta" server

}
